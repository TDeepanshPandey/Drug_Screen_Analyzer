library(shiny)

# UI for the GR Metrics tab
tab1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose Excel File", multiple = FALSE, accept = c(".xlsx")),
    textInput(ns("output_folder_directory"), "Output Folder Directory", "output"),
    tabsetPanel(
      tabPanel("Generate Excel Files",
               checkboxInput(ns("save_gr_metrics_xlsx"), "Save GR Metrics Parameter Table (Excel)", value = FALSE),
               checkboxInput(ns("save_gr_values_xlsx"), "Save Original Data with GR Values (Excel)", value = FALSE),
               actionButton(ns("generate_files"), "Generate Files"),
               downloadButton(ns("download_files"), "Download Files")
      ),
      tabPanel("Generate Graph",
               uiOutput(ns("agent_selector_graph")),  # Dynamic UI for agent selection
               actionButton(ns("generate_individual_graphs"), "Generate Individual Agent Graphs"),
               actionButton(ns("generate_combined_graph"), "Generate Combined Agent Graph")
               #plotOutput(ns("plot"))
      ),
      tabPanel("Display GR Values",
               uiOutput(ns("agent_selector_display")),  # Dynamic UI for agent selection
               actionButton(ns("display_gr_values"), "Displan GR Values for Agent"),
               tableOutput(ns("gr_metrics_table")),  # Table to display the GR metrics
               actionButton(ns("copy_to_clipboard"), "Copy to Clipboard")  # Add copy button
      )
    )
  )
}