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
               actionButton(ns("generate_graph"), "Generate and Save Graph"),
               plotOutput(ns("plot"))
      )
    )
  )
}
