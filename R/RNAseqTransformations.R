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
#' @importFrom shiny NS actionButton icon uiOutput showModal modalDialog


TransformRNAseqDataUI <- function(id) {

  ns <- NS(id)
  tagList(
  selectInput(ns('method'), 'Choose a transformation method to run on your data',
               choices = c('None'='NONE',
                           'Variance Stabilizing Transform (vst)'= 'vst' ,
                           'Regularized logarithm (rlog) - WARNING: this can take considerable time' = 'rlog',
                           "Transcripts per million (tpm)" = "tpm" ),
               selected = 'NONE'),
  selectInput(ns("ShowTransformed"),"Select a counts table to see", choices = c("Rawcounts",'TPM Transformed',
                                                                            'vst Transformed',
                                                                            "Rlog Transformed")),
  fluidRow(DT::dataTableOutput(ns("Table")))


) # end of TagList
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
#' @import rtracklayer


TransformRNAseqDataServer <- function(input, output, session, dds = NULL, minlength = NULL, matrix = NULL, gtf_path = NULL) {

  ns <- session$ns
  reactives <- reactiveValues(tpm = NULL, vst = NULL, rlog = NULL, raw = NULL)

observeEvent(matrix$table,priority = 10,{
  reactives$raw <- matrix$table
  reactives$tpm <-  NULL
  reactives$vst <-  NULL
  reactives$rlog <-  NULL
})

observe({


  withProgress(message = paste0('Counts data ',input$method, ' transformation'),
               detail = 'This may take a while...',
               value = 0,
               {

  if(!is.null(reactives$raw)){

  if (input$method == "tpm"){

    if (!is.null(gtf_path)){

    ExonicSizes <- getExonicGeneSize(gtf_path)
    reactives$tpm <- getTPM(reactives$raw,ExonicSizes)

    } else {

      showModal(modalDialog(
        title = "Missing GTF file",
        "TPM transformation require a GTF file, please provide a path to the Module",
        easyClose = TRUE
      ))


    }


  } else if (input$method == "vst") {

    incProgress(0.4)
    reactives$vst <- vst(as.matrix(reactives$raw),blind=TRUE)
    incProgress(0.4)



  } else if (input$method == "rlog"){

  incProgress(0.4)
  reactives$rlog <- rlog(as.matrix(reactives$raw),blind=TRUE)
  incProgress(0.4)


  }

  } # enf of isnulrawdata

  }) # end of withProgress
}) # end of Observer


observeEvent(c(input$ShowTransformed,reactives$raw),{

  print(head(reactives$vst))
  if (input$ShowTransformed == "Rawcounts"){
    output$Table <- DT::renderDataTable(reactives$raw, options = list(scrollX=TRUE))

  } else if (input$ShowTransformed == "TPM Transformed"){
    output$Table <- DT::renderDataTable(reactives$tpm, options = list(scrollX=TRUE))

  } else if (input$ShowTransformed == "vst Transformed"){
    output$Table <- DT::renderDataTable(reactives$vst, options = list(scrollX=TRUE))

  } else if (input$ShowTransformed == "Rlog Transformed"){
    output$Table <- DT::renderDataTable(reactives$rlog, options = list(scrollX=TRUE))
  }

})

##### Usefull function ############

## getTPM
## Calculate TPM values from a gene expression matrix and a gtf file
## x : matrix of counts
## exonic.gene.size : vector of exonic sizes per gene. If not provided, gtf.in is used to build it
## gtf.in : path to gtf file
## Details : see http://www.rna-seqblog.com/rpkm-fpkm-and-tpm-clearly-explained/ for differences between RPKM and TPM

getTPM <- function(x, exonic.gene.sizes=NULL, gtf.in=NULL){

  stopifnot(require(GenomicFeatures))

  ## First, import the GTF-file that you have also used as input for htseq-count
  if (is.null(exonic.gene.sizes)){
    if (is.null(gtf.in))
      stop("Unable to calculate gene size")
    exonic.gene.sizes <- getExonicGeneSize(gtf.in)
  }

  if (length(setdiff(rownames(x), names(exonic.gene.sizes)))>0){
    warning(length(setdiff(rownames(x), names(exonic.gene.sizes))), " genes from table were not found in the gtf file ...")
  }

  exonic.gene.sizes <- exonic.gene.sizes[rownames(x)]

  ## Calculate read per kilobase
  rpk <- x * 10^3/matrix(exonic.gene.sizes, nrow=nrow(x), ncol=ncol(x), byrow=FALSE)
  ## Then normalize by lib size
  tpm <- rpk *  matrix(10^6 / colSums(rpk, na.rm=TRUE), nrow=nrow(rpk), ncol=ncol(rpk), byrow=TRUE)

  return(round(tpm,2))
}##getTPM


## getExonicGeneSize
## Calcule the exons size per gene for RPMKM/TPM normalization
## gtf.in : path to gtf file
getExonicGeneSize <- function(gtf.in){
  stopifnot(require(GenomicFeatures))
  txdb <- makeTxDbFromGFF(gtf.in,format="gtf")
  ## then collect the exons per gene id
  exons.list.per.gene <- exonsBy(txdb,by="gene")
  ## then for each gene, reduce all the exons to a set of non overlapping exons, calculate their lengths (widths) and sum then
  exonic.gene.sizes <- sum(width(reduce(exons.list.per.gene)))
  return(exonic.gene.sizes)
}## getExonicGeneSize

return(reactives)

}
