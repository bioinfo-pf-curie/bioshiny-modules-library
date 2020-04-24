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
    dashboardHeader(title = "Data transformation Test"),
    dashboardSidebar(),
    dashboardBody(
      TransformRNAseqDataUI("TransformData")
       )
    )



  server <- function(input, output, session) {

    metadata_path <- system.file("extdata", "metadata.csv", package = "BioshinyModules")
    counts_path <- system.file("extdata", "rawcounts.csv", package = "BioshinyModules")


    Transformed <- reactiveValues(rlog = NULL, vts = NULL, rlog = NULL)

    metadata <- reactiveValues(table = read.table(metadata_path, header = TRUE, sep = ",",
                                                  row.names = 1)
    )

    counts <- reactiveValues(table = read.table(counts_path, header = TRUE, sep = ",",
                                                row.names = 1)
    )


 Transformed <- callModule(TransformRNAseqDataServer,"TransformData",session = session,
                               matrix = counts)

#gtf_path <- system.file("extdata", "gencode.v19.annotation.sorted.gtf", package = "PdxAppPackage")


#Transformed <- callModule(TransformRNAseqDataServer,"TransformData",session = session,
#                          matrix = counts, gtf_path = gtf_path)

observe({
print("VST")
print(Transformed$vst)
print("TPM")
print(Transformed$tpm)
print("Rlog")
print(Transformed$rlog)
})



  }
  shinyApp(ui, server)
}
