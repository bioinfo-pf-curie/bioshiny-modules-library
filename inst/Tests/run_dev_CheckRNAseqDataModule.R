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
      ImportDataUIFixed("importData1", label = "Import Counts Matrix"),
      ImportDataUIFixed("importData2", label = "Import Metadata file"),
      fluidRow(column(width = 6 ,CheckRNAseqDataUI(id = "CheckRNAseqData"))),
      fluidRow(DT::dataTableOutput("Table"),
               DT::dataTableOutput("Metadata"))
      )
  )

  server <- function(input, output, session) {


    Rawdata   <- callModule(ImportDataServerFixed, "importData1",
                            session = session,
                            ext = 'txt',
                            sep =",",
                            header = TRUE,
                            row.names = 1)
    #Import Metadata file

    Metadata  <- callModule(ImportDataServerFixed, "importData2",
                            session = session,
                            ext = 'txt',
                            sep =",",
                            header = TRUE,
                            row.names = 1)

    val <- callModule(CheckRNAseqDataServer,"CheckRNAseqData",
                      matrix = Rawdata,
                      metadata = Metadata)

    output$Table <- DT::renderDataTable(Rawdata$table, options = list(scrollX=TRUE))
    output$Metadata <- DT::renderDataTable(Metadata$table, options = list(scrollX=TRUE))

  }
  shinyApp(ui, server)
}
