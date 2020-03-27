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
#' @importFrom shiny NS actionButton icon
#'
#'
#'


CreateDdsUI <- function(id) {
ns <- NS(id)

uiOutput("printdds")

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
#' @importFrom readxl read_excel



CreateDdsServer <- function(input, output, session, matrix = NULL, annotation = NULL) {


  output$printdds <- renderUI({
    shiny::validate(
      need(
        !is.null(values$mydds),
        "Upload your dataset, as a count matrix or passing it as a parameter, as well as the design information"
      )
    )
    verbatimTextOutput("ddsprint")
  })

  output$ddsprint <- renderPrint({
    values$mydds
  })

}

