#' @title Performs normalization on RNAseq data ui part
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


TransformRNAseqDataUI <- function(id) {

  ns <- NS(id)
  selectInput(ns('method'), 'Choose a transformation method for your data',
               choices = c('None'='NONE',
                           'Variance Stabilizing Transform (vst)'= 'vst' ,
                           'Regularized logarithm (rlog) - WARNING: this can take considerable time' = 'rlog',
                           "Transcripts per million (tpm)" = "tpm" ),
               selected = 'NONE')



}


#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#' @title Performs normalization on RNAseq data server part
#' @export
#'
#' @import GenomicFeatures
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @import DESeq2


TransformRNAseqDataServer <- function(input, output, session, dds = NULL, minlength = NULL, matrix = NULL) {

  ns <- session$ns
  reactives <- reactiveValues(tpm = NULL, vst = NULL, rlog = NULL)

observeEvent(input$method,{

  withProgress(message = paste0('Counts data ',input$method, ' transformation'),
               detail = 'This may take a while...',
               value = 0,
               {

  if (input$method == "tpm"){

    #txdb <-
    #transformed <- matrix$table
    # How to deal with transcript length ?

  } else if (input$method == "vst") {


  } else if (input$method == "rlog"){

  incProgress(0.4)
  reactives$rlog <- rlog(as.matrix(matrix$table),blind=TRUE)
  incProgress(0.4)


  }

  }) # end of withProgress
}) # end of Observer

return(reactives)


}
