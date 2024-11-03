tab1_server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  reactive_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    df <- read.xlsx(input$file$datapath)  # Read the Excel file
    df  # Return the dataframe
  })
  
  # Create a reactive value to store filtered metrics
  filtered_metrics <- reactiveVal(NULL)
  
  # Dynamic UI for agent selection in "Generate Graph" tab
  output$agent_selector_graph <- renderUI({
    df <- reactive_data()  # Get the uploaded data
    agents <- unique(df$agent)  # Get the unique agents from the data
    checkboxGroupInput(session$ns("selected_agents_graph"), "Select Agents", choices = agents)
  })
  
  # Dynamic UI for agent selection in "Display GR Values" tab
  output$agent_selector_display <- renderUI({
    df <- reactive_data()  # Get the uploaded data
    agents <- unique(df$agent)  # Get the unique agents from the data
    radioButtons(session$ns("selected_agents_display"), "Select Agent:", choices = agents)
  })
  
  observeEvent(input$generate_files, {
    req(input$file)  # Ensure a file is selected
    
    df <- read.xlsx(input$file$datapath)
    output_dir <- input$output_folder_directory
    
    process_files <- function() {
      tryCatch({
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        
        drc_output = GRfit(df, groupingVariables = c('cell_line','agent'))
        
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
    }
    
    process_files()
  })
  
  # Function to generate and save individual graphs
  observeEvent(input$generate_individual_graphs, {
    req(input$file, input$selected_agents_graph)  # Ensure a file and selected agents are available
    
    df <- read.xlsx(input$file$datapath)
    selected_agents <- input$selected_agents_graph
    output_dir <- input$output_folder_directory
    
    
    process_individual_graphs <- function() {
      tryCatch({
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        
        # Save each plot as a separate file for each selected agent
        for (sel_agent in selected_agents) {
          # Filter data based on selected agents
          filtered_df <- df[df$agent %in% sel_agent, ]
          agent_drc <- GRfit(filtered_df, groupingVariables = c('cell_line', 'agent'))
          p <- customPlotGR(agent_drc)
          
          # Save individual plot
          plot_filepath <- file.path(output_dir, paste0(sel_agent, ".png"))
          message("Saving plot for: ", sel_agent)
          ggsave(plot_filepath, plot = p, width = 16, height = 12, dpi = 300)
        }
        
        showModal(modalDialog(
          title = "Success",
          paste("Individual graphs have been saved in", output_dir)
        ))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message)
        ))
      })
    }
    
    process_individual_graphs()
  })
  
  # Function to generate and save combined graph
  observeEvent(input$generate_combined_graph, {
    
    req(input$file, input$selected_agents_graph)  # Ensure a file and selected agents are available
    
    df <- read.xlsx(input$file$datapath)
    selected_agents <- input$selected_agents_graph
    output_dir <- input$output_folder_directory
    
    # Filter data based on selected agents
    filtered_df <- df[df$agent %in% selected_agents, ]
    
    process_combined_graph <- function() {
      tryCatch({
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        # Fit the model for all selected agents
        drc <- GRfit(filtered_df, groupingVariables = c('cell_line', 'agent'))
        p <- customPlotGR(drc)
        
        # Generate a single plot for all agents
        #output$plot <- renderPlot({
        #  p  # Render the combined plot in Shiny
        #}, height = 600, width = 800)
        
        # Save the combined plot
        combined_plot_filepath <- file.path(output_dir, paste0(paste(selected_agents, collapse = "_"), ".png"))
        print(p)
        ggsave(combined_plot_filepath, plot = p, width = 16, height = 12, dpi = 300)
        
        showModal(modalDialog(
          title = "Success",
          paste("Combined graph has been saved in", output_dir)
        ))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message)
        ))
      })
    }
    
    process_combined_graph()
  })
  
  # Inside your server function (assuming tab1_server or wherever you handle it)
  observeEvent(input$display_gr_values, {
    req(input$file, input$selected_agents_display)   # Ensure an agent is selected
    # Get the selected agent
    selected_agent <- input$selected_agents_display
    
    df <- read.xlsx(input$file$datapath)
    drc_output = GRfit(df, groupingVariables = c('cell_line','agent'))
    
    # Calculate GR metrics using GRgetMetrics function
    gr_metrics <- GRgetMetrics(drc_output)  # assuming drc_output() returns the necessary data
    
    # Filter the metrics for the selected agent
    filtered <- gr_metrics[gr_metrics$agent == selected_agent, ]
    
    # Store the filtered metrics in the reactive value
    filtered_metrics(filtered)
    
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
  })
  
  # Copy entire table to clipboard
  observeEvent(input$copy_to_clipboard, {
    req(filtered_metrics())  # Ensure the table is rendered
    df <- isolate(filtered_metrics())  # Use isolate to avoid reactivity issues
    table_text <- paste(apply(df[, c("cell_line", "GR50", "GR_AOC", "AUC")], 1, function(row) paste(row, collapse = "\t")), collapse = "\n")
    session$sendCustomMessage("copy_table", table_text)
  })
  
  # Add JavaScript for copying individual rows
  session$sendCustomMessage("setup_row_copy", NULL)
}
