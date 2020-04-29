.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
import::from(shinydashboard,box,dashboardPage,dashboardSidebar,dashboardBody,dashboardHeader,DT)


if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "Clustering Module Test"),
    dashboardSidebar(),
    dashboardBody(
      ClusteringUI(id = "heatmapID")
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


Clusterdata <- reactiveValues(table = NULL)

    observe({

    Clusterdata$table <- counts$table[1:50,]

     heatmap <- callModule(ClusteringServer, id = "heatmapID", session = session,
                                    data = Clusterdata, metadata =  metadata, printRows = FALSE)

    })

    output$heatmap <- DT::renderDataTable(heatmap)


  }
  shinyApp(ui, server)
}
