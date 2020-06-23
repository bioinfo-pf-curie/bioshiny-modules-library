.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))
rm(list = ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
library(shiny)
import::from(shinydashboard,box, dashboardPage,dashboardSidebar,dashboardBody,dashboardHeader,sidebarMenu,menuItem,menuSubItem,tabItem,tabPanel)

if (interactive()){

ui <- dashboardPage(
          dashboardHeader(title = "Creates model Test"),
          dashboardSidebar(
            tags$head(
              tags$style(type='text/css', ".span12 { width: 510px; }"),
              tags$style(type='text/css', ".span17 { width: 510px; height = 10000px }")
            ),
            sidebarMenu(id = "sidebartabs",
                                       # menuItem("RNAseq", tabName = "RNAseq",
                                       #          menuSubItem("Data","DataRNA"),
                                       #          menuSubItem(text = "Clustering",tabName = "RNAClustering"),
                                       #          menuSubItem("PCA","PCA"),
                                       #          menuSubItem("DE analysis",tabName = "DEG")
                                       #          )
                                       #div(class = "span17",menuItem("DEG","DEG"))
                                       menuItem("DEG","DEG")
                                       )
          ),
          dashboardBody(
            tabItem("DEG",
            mainPanel(
            tabsetPanel(type = "pills",id = "DEGtabs",
                        tabPanel("RUN DEA",
                                 #div(class="span17",
                          #fluidRow(CreateModelUI("Design"))
                          fluidPage(CreateModelUI("Design"))
                                 #)
                          ),
                        tabPanel("design matrix",
                                 fluidRow(box(title = "Contrasts matrix :",
                                          DT::dataTableOutput("contrast")
                                     )),
                                  fluidRow(box(title = "design matrix",
                                          DT::dataTableOutput("design")))
                                 )
                        )
            )
            )
            )
)

server <- function(input, output, session) {

  #### PDX data # version
    counts_path <- system.file("extdata", "total_pdx_raw_counts_table.csv", package = "PdxAppPackage")
    Metadata <- reactiveValues(table = as.data.frame(read_excel(path = system.file("extdata", "MetadataV2.xlsx", package = "PdxAppPackage"),
                                                              sheet = "metadata")))
    Rawdata <- reactiveValues(table = read.table(counts_path, header = TRUE, sep = ",",row.names = 1))
    MetadataModel <- reactiveValues(table= NULL)
    RawdataModel <- reactiveValues(table =  NULL)
    Clusters <- reactiveValues(table = NULL)
    observe({
      if(!is.null(Metadata$table)){

      a <- Metadata$table
      rownames(a) <- a$PDX_model
      a$PDX_model <- NULL
      a <-a[colnames(Rawdata$table),c("Tumor_subtype","Lehman_classification","Patient_NAC",
                                                   #                       # "AKT1","PIK3CA","NOTCH2","BRCA1",
                                                   #                       # "BRCA2","TP53","NOTCH4","PTEN",
                                                   #                       # "NF1","EGFR","HRAS","GATA3",
                                                   #                       # "ERBB4","PI3KCA","STK11","NRAS",
                                                   #                       # "MAP3K1","PIK3R1","ROS1","BRAF",
                                                   #                       #"TBX3",
                                                   "Acquired_resistance","histology","Brcaness")]

      if(!is.null(Clusters$table)){
        print("addClusters to metadata")
        a$Clusters <- Clusters$table$Clusters[match(rownames(a),rownames(Clusters$table))]
        print(head(a))
      }
      #to_remove <-
      a <- a[which(!(rownames(a) %in% c("NA","NA.1","NA.2","NA.3"))),]
      #a <- subset(a, select = -c("NA","NA.1","NA.2","NA.3"))
      for (col in 1:ncol(a)){
        a[,col] <- as.factor(a[,col])
      }
      #
      #print(setdiff(colnames(RawdataDEA$table),rownames(MetadataDEA$table)))
      if(!is.null(Rawdata$table)){
        #if(!is.null(MetadataDEA$table)){
        RawdataModel$table <- Rawdata$table[,which(colnames(Rawdata$table) %in% rownames(a))]
        print("RawdataModel")
        print(ncol(RawdataModel$table))
        print("MetaModel")
        MetadataModel$table <- a[which(rownames(a) %in% colnames(Rawdata$table)),]
        print(nrow(MetadataModel$table))
        #}
      }

      } # end of if
})

Model <- reactiveValues(contrast = NULL, design = NULL)
rv <- reactiveValues(all_ui = list())
observe({
    Model <- callModule(CreateModelServer, "Design",
                        #sampleplan = Metadata,
                        sampleplan = MetadataModel,
                        matrix = RawdataModel,
                        var= c("Tumor_subtype","Lehman_classification","Patient_NAC",
                               "Acquired_resistance","histology","Brcaness","Clusters"),
                        #var= colnames(MetadataModel),
                        session = session)
      #rv$all_ui[["Design"]] <- CreateModelUI(id = "Design")
    output$contrast <- DT::renderDataTable(

      as.data.frame(Model$contrast)
    )

    output$design <- DT::renderDataTable(
      #req(Model$design),
      as.data.frame(Model$design)
    )
})


#Bioshiny example dataset version
# metadata_path <- system.file("extdata", "metadata.csv", package = "BioshinyModules")
# counts_path <- system.file("extdata", "rawcounts.csv", package = "BioshinyModules")
# metadata <- reactiveValues(table = read.table(metadata_path, header = TRUE, sep = ",",
#                                               row.names = 1))
# counts <- reactiveValues(table = read.table(counts_path, header = TRUE, sep = ",",
#                                             row.names = 1))
# Model <- reactiveValues(contrast = NULL, design = NULL)
# observe({
#     Model <- callModule(CreateModelServer, "Design",
#                         sampleplan = metadata,
#                         matrix = counts,
#                         var = colnames(metadata$table))
# })


#   req(Model$contrast)
    # output$contrast <- DT::renderDataTable(
    #
    #   as.data.frame(Model$contrast)
    # )
    #
    # output$design <- DT::renderDataTable(
    #   #req(Model$design),
    #   as.data.frame(Model$design)
    # )

  }
  shinyApp(ui, server)
}
