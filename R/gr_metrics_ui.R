library(shiny)

# UI for the GR Metrics tab
tab1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Head section with the clipboard handling JavaScript
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('copy_table', function(tableText) {
          // Create a temporary textarea element
          var textarea = document.createElement('textarea');
          textarea.value = tableText;
          document.body.appendChild(textarea);
          textarea.select();
          document.execCommand('copy');
          document.body.removeChild(textarea);
          alert('Full Table Copied!');
        });
        
        // Function to copy individual row
        function copyRow(rowText) {
          var textarea = document.createElement('textarea');
          textarea.value = rowText;
          document.body.appendChild(textarea);
          textarea.select();
          document.execCommand('copy');
          document.body.removeChild(textarea);
        }
      "))
    ),
    fileInput(ns("file"), "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
    textInput(ns("output_folder_directory"), "Output Folder Directory", "output"),
    
    # Main tabset panel - always show it, don't use conditionalPanel
    tabsetPanel(
      tabPanel("Generate Output Excel",
              checkboxInput(ns("save_gr_metrics_xlsx"), "Save GR Metrics Parameter Table (Excel)", value = FALSE),
              checkboxInput(ns("save_gr_values_xlsx"), "Save Original Data with GR Values (Excel)", value = FALSE),
              actionButton(ns("generate_files"), "Generate Files"),
              downloadButton(ns("download_files"), "Download Files")
      ),
      tabPanel("Generate Graph",
              checkboxInput(ns("show_mean_only"), "Show Mean Values Only", value = FALSE),
              uiOutput(ns("agent_selector_graph")),  # Dynamic UI for agent selection
              uiOutput(ns("cell_line_selector_graph")),  # Dynamic UI for cell line selection
              actionButton(ns("generate_individual_graphs"), "Generate Individual Treatment Graphs"),
              actionButton(ns("generate_combined_graph"), "Generate Combined Treatment Graph")
      ),
      tabPanel("Display GR Values",
              uiOutput(ns("agent_selector_display")),  # Dynamic UI for agent selection
              actionButton(ns("display_gr_values"), "Display GR Values for Agent"),
              actionButton(ns("copy_to_clipboard"), "Copy Entire Table"),  # Add copy button
              DTOutput(ns("gr_metrics_table"))  # Table to display the GR metrics
      )
    )
  )
}