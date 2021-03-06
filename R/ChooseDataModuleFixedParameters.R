#' @title Module for importing text of xls files ui part
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
ImportDataUIFixed <- function(id, label = "Import Data") {
  ns <- NS(id)
  tagList(
    fileInput(ns("button"),label = label,
              accept=c('application/vnd.ms-excel',
                       'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                       '.xls',
                       '.xlsx',
                       '.txt',
                       '.csv',
                       '.tsv')
              )

  )# End of TagList
}


#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#' @title Module for importing text of xls files server part
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom readxl read_excel



ImportDataServerFixed <- function(input, output, session, header = NULL, sep  = NULL, ext = 'txt', row.names = NULL )  {

  ns <- session$ns
  data <- reactiveValues(table = NULL , name = NULL, trigger = NULL)
  # data_table <- reactive({NULL})

  observeEvent(input$button, {

  req(input$button)
  infile <- input$button
   if ( ext == 'txt') {

     data$table <- read.table(infile$datapath, header = header, sep = sep, row.names = row.names)
     data$name <- infile$name

   } else if (ext == "xls"
              | ext == "xlsx"){


     data$table <- read_excel(path = infile$datapath)

   }

})

return(data)

}

