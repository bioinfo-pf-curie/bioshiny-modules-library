.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
import::from(shinydashboard,box,dashboardPage,dashboardSidebar,dashboardBody,dashboardHeader,DT)


if (interactive()){

  library(shinydashboard)
  ui <- dashboardPage(
    dashboardHeader(title = "Clustering Module Test"),
    dashboardSidebar(sidebarMenu(menuItem("CNV", tabName = "CNV",
                                          menuSubItem("Data",tabName = "DataCNV"),
                                          menuSubItem("Clustering",tabName = "ClusteringCNV")))),
    dashboardBody(
      tabItem(tabName = "ClusteringCNV",
      fluidRow(
      ClusteringUICNV("ClusterCNV")
      )# end of tabItem
      ) # end of FluidPage
    )
  )

  server <- function(input, output, session) {

    library(dplyr)
    inFile <- system.file("extdata", "CNV_per_genes_table.RDA",package = "PdxAppPackage")
    load(inFile)

    CNVploidy <- as.data.frame(read.table(system.file("extdata", "PloidyCNV.txt", package = "PdxAppPackage"), header = TRUE))

    #observeEvent(CNVtabsub$matrix,ignoreInit = TRUE,{

      print("CNVploidy")
      print(head(CNVploidy))

      # CNVtabsub <- CNVtabmatrix %>%
      #   select(c("GENE","sample","GNL","CnGap"))

      CNVmatrixspread <- CNVtabmatrix[,c("GENE","CnGap","sample")] %>% distinct(GENE,sample,.keep_all = TRUE) %>% tidyr::spread(sample,CnGap)
      rownames(CNVmatrixspread) <- CNVmatrixspread$GENE
      CNVmatrixspread$GENE <- NULL
      #print("matrixspread")
      #print(CNVmatrixspread)
      ## Remove Ploidy from absolute Cn value
      head(CNVploidy)
      missingPloidy <- NULL
      for (col in colnames(CNVmatrixspread)){
        #print("Sample :")
        #print(col)
        if(col %in% CNVploidy$Sample){
          #print("Ploidy :")
          #print(as.numeric(CNVploidy[which(CNVploidy$Sample == as.character(col)),2]))
          CNVmatrixspread[,col] <- CNVmatrixspread[,col] - as.numeric(CNVploidy[which(CNVploidy$Sample == as.character(col)),2])
        } else {
          print("no ploidy data")
          CNVmatrixspread[,col] <- NULL
        }

      }
      #####

    metaCNV <- data.frame(Sample = unique(CNVploidy$Sample))
    rownames(metaCNV) <- metaCNV$Sample
    metaCNV$Sample <- NULL
    metaCNV$Group1 <- 'a'
    metaCNV$Group2 <- 'b'

    # print("MetaCNV")
    # print(head(metaCNV))



    observe({


    CNVmatrixspread <- reactiveValues(table = na.omit(CNVmatrixspread[1:50,]))
    metadataClustCNV <- reactiveValues(table = metaCNV )

    if (!is.null(metadataClustCNV) & !is.null(CNVmatrixspread)){


    CNVclust <- callModule(ClusteringServerCNV,"ClusterCNV",
                           data = CNVmatrixspread, metadata = metadataClustCNV,printRows = FALSE,session = session)

    } # end of IF

    })
    #
    #
    #
    #})





  }
  shinyApp(ui, server)
}
