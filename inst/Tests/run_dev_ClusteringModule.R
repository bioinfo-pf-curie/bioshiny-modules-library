.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
import::from(shinydashboard,box,dashboardPage,dashboardSidebar,dashboardBody,dashboardHeader)
library(DT)
library(DESeq2)


if (interactive()){

  ui <- dashboardPage(
    dashboardHeader(title = "Clustering Module Test"),
    dashboardSidebar(),
    dashboardBody(
      fluidRow(
      ClusteringUI(id = "heatmapID")
      ) # end of FluidPage
    )
  )

  server <- function(input, output, session) {


    metadata_path <- system.file("extdata", "metadata.csv", package = "BioshinyModules")
    counts_path <- system.file("extdata", "rawcounts.csv", package = "BioshinyModules")
    #vst_path <- system.file("extdata", "vst.csv", package = "BioshinyModules")


    metadata <- reactiveValues(table = read.table(metadata_path, header = TRUE, sep = ",",
                                                  row.names = 1)
    )
    counts <- reactiveValues(table = read.table(counts_path, header = TRUE, sep = ",",
                                                row.names = 1)[1:50,]
    )
    vst <- reactiveValues(vars = vst(as.matrix(na.omit(read.table(counts_path, header = TRUE, sep = ",",
                                                          row.names = 1))),blind = TRUE))
    # counts <-  read.table(counts_path, header = TRUE, sep = ",",
    #                       row.names = 1)
    # metadata <- read.table(metadata_path, header = TRUE, sep = ",",
    #                        row.names = 1)
    # vst <- vst(as.matrix(counts),blind = TRUE)[1:50,]

    # vst <- reactiveValues(table = read.table(vst_path, header = TRUE, sep = ",",
    #                                             row.names = 1)



# vst <- reactiveValues(table = NULL)
#     observe({
#
#    # Clusterdata <- reactiveValues(table =  counts$table[1:50,])
#       print("vst")
#     vst$table <- vst(as.matrix(counts$table),blind = TRUE)[1:50,]
#
#     print("vst")
# })
   # print("launch module pca")
    observe({
      #print(class(vst))
      print("HEATMAP")
     #heatmap <-
      isolate(
       heatmap <- callModule(ClusteringServer, id = "heatmapID", session = session,
                                    data = counts, metadata =  metadata, printRows = FALSE,
                           vst = vst)
)
       })
 #   })

    output$heatmap <- DT::renderDataTable(heatmap)


  }
  shinyApp(ui, server)
}

