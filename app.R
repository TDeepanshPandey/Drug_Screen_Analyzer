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

# Create a global object to store app state across sessions
if(!exists("global_state")) {
  global_state <- new.env()
  global_state$app_data <- list()
}

# Set session options for stability
options(shiny.maxRequestSize = 100 * 1024^2)  # Increase max upload size to 100MB

# Source the UI and server logic for the tab
source("R/gr_metrics_ui.R")
source("R/gr_metrics_server.R")
source("R/custom_plot_functions.R")

# Main UI
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      // Prevent accidental page refresh
      window.addEventListener('beforeunload', function(e) {
        e.preventDefault();
        e.returnValue = '';
      });
      
      // Keep session alive with periodic pings
      setInterval(function() {
        Shiny.setInputValue('keep_alive', new Date().getTime());
      }, 30000);
    "))
  ),
  titlePanel("Drug Screen Analyzer"),
  tabsetPanel(
    tabPanel("GR Metrics", tab1_ui("gr_metrics"))
  )
)

# Main Server
server <- function(input, output, session) {
  # Session keep-alive mechanism
  observeEvent(input$keep_alive, {
    # This keeps the session alive
  })
  
  # Call the module server with global state
  callModule(tab1_server, "gr_metrics", global_state)
  
  # Handle session end
  session$onSessionEnded(function() {
    # Session cleanup code can go here if needed
  })
}

# Run the application
shinyApp(ui = ui, server = server)