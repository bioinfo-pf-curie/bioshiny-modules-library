#' @title Filter counts data for further analysis UI side
#'
#' @description
#'
#' @param id Module's id.
#' @param label Button's label.
#' @param icon Button's icon.
#' @param ... Arguments passed to \code{\link{actionButton}}
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data selected under slot \code{data}
#' and the name of the selected \code{data.frame} under slot \code{name}.
#' @export
#'
#'
#' @importFrom esquisse filterDF_UI
#' @importFrom shiny NS actionButton icon uiOutput
#' @importFrom shinyWidgets progressBar updateProgressBar
#' @import DT
#'
#'


FilterRNAUI <- function(id){

  ns <- NS(id)
  tagList(
    # numericInput(inputId = ns("TPM_sum"),
    #               label = "Select features with TPM sum >= to : ", min = 0, max = 20, value = 10),
    # numericInput(inputId = ns("TPM_mean"),
     #            label = "Select features with TPM mean >= to : ", min = 0, max = 20, value = 2),
    numericInput(inputId = ns("TPM_filter"),
                 label = "Select features with TPM value >= to : ", min = 0, max = 20, value = 2),
    numericInput(inputId = ns("nsamples"),
                 label = "in at least 'value' % of samples : ", min = 0, max = 20, value = 10),
    fluidRow(
      column(
      width = 9,
      progressBar(
        id = ns("pbar"), value = 100,
        total = 100, display_pct = TRUE
      ),
      tags$h1("Filtered data table :"),
      DT::dataTableOutput(outputId = ns("table")),
     ) # end of column
    ) # end of FluidRow

  ) # end of TagList

}


#' @param input,output,session standards \code{shiny} server arguments.
#' @param name Does the file have a Header
#' @param data A reactiveValues containing the table reactiveValue with table a dataframe. Example data$table  a reactive dataframe
#'
#' @export
#'
#'
#' @title Filter counts data for further analysiss server side
#' @importFrom shiny observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML


FilterRNAServer <- function(input, output, session, data = NULL) {

  ns <- session$ns
  rawdata <- reactiveValues(table = NULL)
  returns <- reactiveValues(DataFiltered = NULL)

  observeEvent(c(data$table,
                 #input$TPM_mean,
                 #input$TPM_sum
                 input$TPM_filter,
                 input$nsamples),{

  if (!is.null(data$table)){

  data <- data$table
  isexpr <- names(which(apply(data, 1, function(x){length(which(x>=input$TPM_filter))})>=(length(colnames(data))*input$nsamples/100)))
  #rawdata$table[,"TPM_sum"] <- rowSums(rawdata$table)
  #rawdata$table[,"TPM_mean"] <- rowMeans(rawdata$table[,-ncol(rawdata$table)])
  # returns$DataFiltered <- rawdata$table[which(rawdata$table[,'TPM_sum'] >= input$TPM_sum),]
  # returns$DataFiltered <- returns$DataFiltered[which(returns$DataFiltered[,'TPM_mean'] >= input$TPM_mean),]
  # returns$DataFiltered <- returns$DataFiltered[,-c(ncol(returns$DataFiltered),
  #                                                ncol(returns$DataFiltered) - 1)
  returns$DataFiltered <-  data[isexpr,]
  } # end of if

  })


  observeEvent(returns$DataFiltered, {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(returns$DataFiltered), total = nrow(data$table),
      title = "Number of features after filetering step :"
    )
})

    output$table <- DT::renderDataTable({
      returns$DataFiltered
    }, options = list(pageLength = 10,scrollX = TRUE))


 # end of observer


  return(returns)
}

