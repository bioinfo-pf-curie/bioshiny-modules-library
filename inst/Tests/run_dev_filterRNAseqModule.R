.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
library(BioshinyModules)
library(cicerone)
import::from(shinydashboard,box,dashboardPage,dashboardSidebar,dashboardBody,dashboardHeader,DT)

if (interactive()){
  ui <- dashboardPage(
    dashboardHeader(title = "Filter RNAseq Test"),
    dashboardSidebar(),
    dashboardBody(
      fluidPage(
      list(use_cicerone()),
      FilterRNAUI(id = "filtRnaSeq"))
    )
  )

  server <- function(input, output, session) {

    metadata_path <- system.file("extdata", "metadata.csv", package = "BioshinyModules")
    counts_path <- system.file("extdata", "rawcounts.csv", package = "BioshinyModules")
    metadata <- reactiveValues(table = read.table(metadata_path, header = TRUE, sep = ",",
                                                  row.names = 1)
    )
    counts <- reactiveValues(table = read.table(counts_path, header = TRUE, sep = ",",
                                                row.names = 1)
    )

    filtered_counts <- callModule(FilterRNAServer, id = "filtRnaSeq", session = session,
                       data = counts)
    
  # observe({
  #   if(exists("filtered_counts")){
  #   print(filtered_counts$DataFiltered)
  #   }
  # })
  }
  shinyApp(ui, server)
}
