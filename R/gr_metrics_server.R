library(shiny)
library(openxlsx)
library(GRmetrics)
source("custom_plot_functions.R")

tab1_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$generate_files, {
    req(input$file)  # Ensure a file is selected
    
    process_files <- function() {
      tryCatch({
        df <- read.xlsx(input$file$datapath)
        drc_output <- GRfit(df, groupingVariables = c('cell_line', 'agent'))
        
        output_dir <- input$output_folder_directory
        
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        
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
  
  observeEvent(input$generate_graph, {
    req(input$file)  # Ensure a file is selected
    
    process_graphs <- function() {
      tryCatch({
        df <- read.xlsx(input$file$datapath)
        drc_output <- GRfit(df, groupingVariables = c('cell_line', 'agent'))
        
        output_dir <- input$output_folder_directory
        
        if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
        }
        
        p <- customPlotGR(drc_output)
        
        output$plot <- renderPlot({
          print(p)  # Print the plot object to render it in Shiny
        }, height = 600, width = 800)
        
        plot_filepath <- file.path(output_dir, "agent.png")
        ggsave(plot_filepath, plot = p, width = 16, height = 12, dpi = 300)  # Save the plot
        
        showModal(modalDialog(
          title = "Success",
          paste("Graph has been saved in", output_dir)
        ))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred:", e$message)
        ))
      })
    }
    
    process_graphs()
  })
}
