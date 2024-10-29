# List of required packages
required_packages <- c("shiny", "openxlsx", "GRmetrics", "ggplot2", "DT")

# Function to check and install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages)
  }
}

# Check and install missing packages
install_if_missing(required_packages)

# Install and load GRmetrics using BiocManager if it does not exist
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Check if GRmetrics is installed; if not, install it
if (!require("GRmetrics", quietly = TRUE)) {
  BiocManager::install("GRmetrics")
}

library(shiny)
library(openxlsx)
library(GRmetrics)
library(ggplot2)

# Source the UI and server logic for the tab
source("R/gr_metrics_ui.R")
source("R/gr_metrics_server.R")
source("R/custom_plot_functions.R")

# Main UI
ui <- fluidPage(
  titlePanel("Drug Screen Analyzer"),
  tabsetPanel(
    tabPanel("GR Metrics", tab1_ui("gr_metrics"))
  )
)

# Main Server
server <- function(input, output, session) {
  callModule(tab1_server, "gr_metrics")
}

# Run the application
shinyApp(ui = ui, server = server)