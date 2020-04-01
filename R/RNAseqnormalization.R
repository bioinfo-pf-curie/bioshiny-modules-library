#' @title performs normalization on RNAseq data
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


NormalizeRNAseqDataUI <- function(id) {

  radioButtons(ns('normalization'), 'Normalization',
               choices = c('None'='NONE',
                           'Variance Stabilizing Transform (vst)'='vst',
                           'Regularized logarithm (rlog) - WARNING: this can take considerable time'='rlog'),
               selected = 'NONE')



}



NormalizeRNAseqDataServer <- function(input, output, session, matrix = NULL) {








}
