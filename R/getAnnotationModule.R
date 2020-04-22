#' @title Modules getting a bioMaRt annotation from genes's list ui part
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


getAnnotationUI <- function(id) {
  ns <- NS(id)


}




#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#' @title Modules getting a bioMaRt annotation from genes's list server part
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom readxl read_excel



getAnnotationServer <- function(input, output, session, matrix = NULL, annotation = NULL) {

output$printanno <- renderUI({
  shiny::validate(
    need(
      !is.null(values$myannotation),
      "Upload your annotation table as a matrix/data frame or passing it as a parameter"
    )
  )
  DT::dataTableOutput("annoprint")
})


}
