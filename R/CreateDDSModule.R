#' @title Creates a DDS object for further analysis.
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
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS actionButton icon uiOutput
#'
#'
#'


CreateDdsUI <- function(id)  {
ns <- NS(id)
tagList(
  fluidRow(uiOutput(ns("CreateButtonUI"))),
  fluidRow(uiOutput(ns("printdds")))
)

}


#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom DESeq2 DESeqDataSetFromMatrix


CreateDdsServer <- function(input, output, session, countmatrix = NULL, coldata = NULL) {

  req(countmatrix)
  ns <- session$ns

  ### Defin reactives #############
  reactives <-  reactiveValues(mydds = NULL)


  output$CreateButtonUI <- renderUI({
    if (is.null(countmatrix) | is.null(coldata)) {
      shinyjs::disabled(
        actionButton(ns("CreateButton"), label = "Create DDS object")
      )
    } else {
     actionButton(ns("CreateButton"), label = "Create DDS object")
    }
  })


  output$printdds <- renderUI({
    shiny::validate(
      need(
        !is.null(reactives$mydds),
        "Upload your dataset, as a count matrix or passing it as a parameter, as well as the design information"
      )
    )
    verbatimTextOutput("ddsprint")
  })

  output$ddsprint <- renderPrint({
    reactives$mydds
  })



  observeEvent(input$CreateButton,{

    if (is.null(countmatrix) | is.null(coldata)) {
      return(NULL)

    } else {

      dds <- DESeqDataSetFromMatrix(
        countData = countmatrix,
        colData = coldata,
        design = ~ 1
      )
      dds <- estimateSizeFactors(dds)

    }

  if (!is.null(dds)) {
    if (!is(dds, "DESeqDataSet")) {
      stop("dds must be a DESeqDataSet object. If it is a simple counts matrix, provide it to the countmatrix parameter!")
    }
  }

  if (!is.null(dds)) {
    if (is.null(sizeFactors(dds))) {
      dds <- estimateSizeFactors(dds)
    }
  }

  reactives$mydds <- dds

  })

  return(reactives$mydds)
}

