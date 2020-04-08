.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
import::from(shinydashboard,box, dashboardPage,dashboardSidebar,dashboardBody,dashboardHeader)


if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "Creates model Test"),
    dashboardSidebar(),
    dashboardBody(
      CreateDdsUI(id = "DDSRnaSeq")#,
     #textOutput("dds")
      # fluidRow(box(title = "Contrasts matrix :",width =12,
      #              DT::dataTableOutput("contrast")))
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


    dds <- callModule(CreateDdsServer, "DDSRnaSeq", session = session,
                      countmatrix = counts,
                      colData = metadata)


    #output$dds <- renderText(renderPrint(print(dds$mydds)))


  }
  shinyApp(ui, server)
}
