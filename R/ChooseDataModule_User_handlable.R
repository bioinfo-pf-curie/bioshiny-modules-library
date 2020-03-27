#' @title Module for importing text of xls files
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
#' @name module-ImportData
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shiny NS actionButton icon
#'
ImportDataUI <- function(id, label = "Import Data") {
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

  )
}


#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#' @rdname module-chooseData
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom assertthat has_extension see_if



ImportDataServer <- function(input, output, session) {

  ns <- session$ns
  data <- reactiveValues(table = NULL , name = NULL)


  observeEvent(input$button, {

    req(input$button)
    dataModal <- function(failed = FALSE ) {
      modalDialog(easyClose =TRUE ,
                  selectInput(inputId  = ns("sep"), label = "File Separator",
                              choices = c(",","\\t",";"), selected = ","),
                  selectInput(inputId = ns("Head"), label = "Header",
                              choices = c("Yes","No"),
                              selected = "Yes"),
                  #footer = modalButton("Use this parameters"),
                  footer = actionButton(ns("OKbutton"),"Use this parameters")

      )
    }

  infile <- input$button
   if (see_if(has_extension(infile$datapath, 'csv')) == TRUE
      | see_if(has_extension(infile$datapath, 'tsv')) == TRUE
      | see_if(has_extension(infile$datapath, 'txt')) == TRUE
      | see_if(has_extension(infile$datapath, 'CSV')) == TRUE
      | see_if(has_extension(infile$datapath, 'TSV')) == TRUE
      | see_if(has_extension(infile$datapath, 'TXT')) == TRUE
      | see_if(has_extension(infile$datapath, 'CSV')) == TRUE) {

      showModal(dataModal())

   }
  observeEvent(input$OKbutton,{
  removeModal()
  if (input$Head == "Yes") {
    Head <- TRUE
  } else if (input$Head == "No") {
    Head <- FALSE
  }
  data$table <- read.table(infile$datapath, header = Head, sep = input$sep)
  })

  })

  return(data)

}

