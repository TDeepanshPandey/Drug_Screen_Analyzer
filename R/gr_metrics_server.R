library(DT)  

tab1_server <- function(input, output, session, global_state) {
  
  # Create reactive values to store state within the session
  rv <- reactiveValues(
    data = NULL,
    metrics = NULL,
    processed_drc = NULL,
    file_path = NULL
  )
  
  # Save state to global environment
  saveState <- function() {
    global_id <- session$ns("data")
    isolate({
      global_state$app_data[[global_id]] <- list(
        data = rv$data,
        metrics = rv$metrics,
        processed_drc = rv$processed_drc,
        file_path = rv$file_path
      )
    })
  }
  
  # Load state from global environment
  loadState <- function() {
    global_id <- session$ns("data")
    if (!is.null(global_state$app_data[[global_id]])) {
      data <- global_state$app_data[[global_id]]
      rv$data <- data$data
      rv$metrics <- data$metrics
      rv$processed_drc <- data$processed_drc
      rv$file_path <- data$file_path
      return(TRUE)
    }
    return(FALSE)
  }
  
  # Try to load state on initialization
  observe({
    # This runs once when the session starts
    loadState()
  }, priority = 1000)
  
  # Reactive expression to read the uploaded file
  observeEvent(input$file, {
    req(input$file)  # Ensure the file is uploaded
    
    # Only read the file if it's new or data is NULL
    isolate({
      if(is.null(rv$data) || input$file$datapath != rv$file_path) {
        # Show progress during file loading
        withProgress(message = 'Loading file...', {
          tryCatch({
            rv$data <- read.xlsx(input$file$datapath)
            rv$file_path <- input$file$datapath
            
            # Pre-process the data to avoid recalculation
            incProgress(0.5, detail = "Pre-processing data")
            rv$processed_drc <- GRfit(rv$data, groupingVariables = c('cell_line','agent'))
            
            # Save state after successful processing
            saveState()
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              paste("Failed to process data:", e$message)
            ))
          })
        })
      }
    })
  })
  
  # Dynamic UI for agent selection in "Generate Graph" tab
  output$agent_selector_graph <- renderUI({
    req(rv$data)  # Get the uploaded data from reactive values
    agents <- unique(rv$data$agent)  # Get the unique agents from the data
    checkboxGroupInput(session$ns("selected_agents_graph"), "Select Treatment", choices = agents)
  })
  
  # Dynamic UI for cell line selection in "Generate Graph" tab
  output$cell_line_selector_graph <- renderUI({
    req(rv$data)  # Get the uploaded data from reactive values
    cell_lines <- unique(rv$data$cell_line)  # Get the unique cell lines from the data
    checkboxGroupInput(session$ns("selected_cell_lines_graph"), "Select Cell Lines", 
                       choices = cell_lines, selected = cell_lines)  # Default: all selected
  })
  
  # Dynamic UI for agent selection in "Display GR Values" tab
  output$agent_selector_display <- renderUI({
    req(rv$data)  # Get the uploaded data from reactive values
    agents <- unique(rv$data$agent)  # Get the unique agents from the data
    radioButtons(session$ns("selected_agents_display"), "Select Treatment:", choices = agents)
  })
  
  observeEvent(input$generate_files, {
    req(rv$data)  # Ensure data is available in reactive values
    
    output_dir <- input$output_folder_directory
    
    withProgress(message = 'Generating files...', {
      tryCatch({
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        
        # Use pre-processed DRC output if available
        drc_output <- rv$processed_drc
        
        # Save GR metrics parameter table as Excel if requested
        if (input$save_gr_metrics_xlsx) {
          gr_metrics_filepath_xlsx <- file.path(output_dir, "gr_metrics.xlsx")
          write.xlsx(GRgetMetrics(drc_output), file = gr_metrics_filepath_xlsx, rowNames = FALSE)
        }
        
        # Save original data with GR values as Excel if requested
        if (input$save_gr_values_xlsx) {
          gr_values_filepath_xlsx <- file.path(output_dir, "gr_values.xlsx")
          write.xlsx(GRgetValues(drc_output), file = gr_values_filepath_xlsx, rowNames = FALSE)
        }
        
        # Provide a zip file of all generated files for download
        output$download_files <- downloadHandler(
          filename = function() {
            paste("files.zip")
          },
          content = function(file) {
            files_to_zip <- list.files(output_dir, full.names = TRUE, pattern = "\\.xlsx$")
            zip(file, files_to_zip)
          }
        )
        
        showModal(modalDialog(
          title = "Success",
          paste("Files have been saved in", output_dir)
        ))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message)
        ))
      })
    })
  })
  
  # Function to generate and save individual graphs - with onFlushed to maintain state
  observeEvent(input$generate_individual_graphs, {
    # Use req() separately for better error handling
    req(rv$data)
    req(input$selected_agents_graph)
    req(input$selected_cell_lines_graph)
    
    selected_agents <- input$selected_agents_graph
    selected_cell_lines <- input$selected_cell_lines_graph
    output_dir <- input$output_folder_directory
    show_mean_only <- input$show_mean_only
    
    # Show progress during generation
    withProgress(message = 'Generating individual graphs...', value = 0, {
      tryCatch({
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        
        total_agents <- length(selected_agents)
        
        # Save each plot as a separate file for each selected agent
        for (i in seq_along(selected_agents)) {
          sel_agent <- selected_agents[i]
          incProgress(1/total_agents, detail = paste("Processing", sel_agent))
          
          # Filter data based on selected agents and cell lines
          filtered_df <- rv$data[rv$data$agent %in% sel_agent & rv$data$cell_line %in% selected_cell_lines, ]
          agent_drc <- GRfit(filtered_df, groupingVariables = c('cell_line', 'agent'))
          
          # Pass the show_mean_only parameter to the plotting function
          p <- customPlotGR(agent_drc, show_mean_only = show_mean_only)
          
          # Create filename with agent and selected cell lines
          filename_suffix <- if(length(selected_cell_lines) < length(unique(rv$data$cell_line))) {
            paste0("_", paste(selected_cell_lines, collapse="_"))
          } else {
            ""  # If all cell lines are selected, don't add suffix
          }
          
          # Add mean indicator to filename if showing means only
          if(show_mean_only) {
            filename_suffix <- paste0(filename_suffix, "_mean")
          }
          
          # Save individual plot
          plot_filepath <- file.path(output_dir, paste0(sel_agent, filename_suffix, ".png"))
          ggsave(plot_filepath, plot = p, width = 16, height = 12, dpi = 300)
        }
        
        # Use onFlushed to make sure this code runs after UI updates
        session$onFlushed(function() {
          # Save state after successful processing
          saveState()
          
          # Show success message
          showModal(modalDialog(
            title = "Success",
            paste("Individual graphs have been saved in", output_dir)
          ))
        })
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message)
        ))
      })
    })
  })
  
  # Function to generate and save combined graph - with onFlushed to maintain state
  observeEvent(input$generate_combined_graph, {
    # Use req() separately for better error handling
    req(rv$data)
    req(input$selected_agents_graph)
    req(input$selected_cell_lines_graph)
    
    selected_agents <- input$selected_agents_graph
    selected_cell_lines <- input$selected_cell_lines_graph
    output_dir <- input$output_folder_directory
    show_mean_only <- input$show_mean_only
    
    # Filter data based on selected agents and cell lines
    filtered_df <- rv$data[rv$data$agent %in% selected_agents & rv$data$cell_line %in% selected_cell_lines, ]
    
    withProgress(message = 'Generating combined graph...', {
      tryCatch({
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        # Fit the model for all selected agents and cell lines
        drc <- GRfit(filtered_df, groupingVariables = c('cell_line', 'agent'))
        
        # Pass the show_mean_only parameter to the plotting function
        p <- customPlotGR(drc, show_mean_only = show_mean_only)
        
        # Create filename with selected agents and cell lines
        filename_prefix <- paste(selected_agents, collapse="_")
        filename_suffix <- if(length(selected_cell_lines) < length(unique(rv$data$cell_line))) {
          paste0("_", paste(selected_cell_lines, collapse="_"))
        } else {
          ""  # If all cell lines are selected, don't add suffix
        }
        
        # Add mean indicator to filename if showing means only
        if(show_mean_only) {
          filename_suffix <- paste0(filename_suffix, "_mean")
        }
        
        # Save the combined plot
        combined_plot_filepath <- file.path(output_dir, paste0(filename_prefix, filename_suffix, ".png"))
        ggsave(combined_plot_filepath, plot = p, width = 16, height = 12, dpi = 300)
        
        # Use onFlushed to make sure this code runs after UI updates
        session$onFlushed(function() {
          # Save state after successful processing
          saveState()
          
          # Show success message
          showModal(modalDialog(
            title = "Success",
            paste("Combined graph has been saved in", output_dir)
          ))
        })
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message)
        ))
      })
    })
  })
  
  # Inside your server function (assuming tab1_server or wherever you handle it)
  observeEvent(input$display_gr_values, {
    req(rv$data)
    req(input$selected_agents_display)
    
    # Get the selected agent
    selected_agent <- input$selected_agents_display
    
    # Use pre-processed DRC output if available
    drc_output <- rv$processed_drc
    
    # Calculate GR metrics using GRgetMetrics function
    gr_metrics <- GRgetMetrics(drc_output)
    
    # Filter the metrics for the selected agent
    filtered <- gr_metrics[gr_metrics$agent == selected_agent, ]
    
    # Store the filtered metrics in the reactive value
    rv$metrics <- filtered
    
    # Define a custom formatting function
    format_number <- function(x) {
      if (abs(x) < 1e10) {
        return(format(x, nsmall = 2))  # Normal format with two decimal places
      } else {
        return(format(x, scientific = TRUE, digits = 4))  # Scientific format
      }
    }
    
    filtered$GR50 <- sapply(filtered$GR50, format_number)
    filtered$GR_AOC <- sapply(filtered$GR_AOC, format_number)
    filtered$AUC <- sapply(filtered$AUC, format_number)
    
    # Render DataTable with individual copy buttons
    output$gr_metrics_table <- renderDT({
      filtered$Copy <- sapply(1:nrow(filtered), function(i) {
        row_text <- paste(filtered[i, c("cell_line", "GR50", "GR_AOC", "AUC")], collapse = "\t")
        sprintf('<button onclick="copyRow(`%s`)">Copy</button>', row_text)
      })
      datatable(
        filtered[, c("cell_line", "GR50", "GR_AOC", "AUC", "Copy")],
        escape = FALSE, rownames = FALSE,
        selection = 'none',
        options = list(dom = 't', paging = FALSE, ordering = FALSE)
      )
    })
    
    # Save state after successful processing
    saveState()
  })
  
  # Copy entire table to clipboard
  observeEvent(input$copy_to_clipboard, {
    req(rv$metrics)  # Use metrics from reactive values
    df <- isolate(rv$metrics)  # Use isolate to avoid reactivity issues
    table_text <- paste(apply(df[, c("cell_line", "GR50", "GR_AOC", "AUC")], 1, function(row) paste(row, collapse = "\t")), collapse = "\n")
    session$sendCustomMessage("copy_table", table_text)
  })
  
  # Add JavaScript for copying individual rows
  session$sendCustomMessage("setup_row_copy", NULL)
  
  # Make sure outputs don't get suspended
  outputOptions(output, "agent_selector_graph", suspendWhenHidden = FALSE)
  outputOptions(output, "cell_line_selector_graph", suspendWhenHidden = FALSE)
  outputOptions(output, "agent_selector_display", suspendWhenHidden = FALSE)
  
  # Keep reactive data alive even when not being accessed
  observe({
    list(rv$data, rv$processed_drc, rv$metrics, rv$file_path)
    # This observe block keeps reactives alive by referencing them
  })
}
