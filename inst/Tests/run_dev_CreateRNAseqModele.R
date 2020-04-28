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
             fluidRow(CreateModelUI("Design")),
             fluidRow(box(title = "Contrasts matrix :",width =12,
                 column(DT::dataTableOutput("contrast")))
                 ),
                column(DT::dataTableOutput("design"))
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


    Model <- callModule(CreateModelServer, "Design",
                        sampleplan = metadata,
                        matrix = counts)

    output$contrast <- DT::renderDataTable(

      as.data.frame(Model$contrast)
    )

    output$design <- DT::renderDataTable(

      as.data.frame(Model$design)
    )


  }
  shinyApp(ui, server)
}
