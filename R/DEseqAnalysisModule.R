#' @title Perform DE analysis ui side
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


DEAUI <- function(id)  {
ns <- NS(id)
tagList(
selectInput(ns("DEAmeth"),"Select a package for DEA analysis",choices = c("DEseq2","limma"), selected = "limma"),
selectInput(ns("transformed"),"Perform DEA on ",choices = c("Raw data","Rlog Data","VST Data")),
selectInput(ns("AdjMeth"),"Select an adjustment method", choices = c("BH","none","BY","holm"), selected = "BH"),
numericInput(ns("PvalsT"),"P values threshold", min = 0, max = 1 , value = 0.05, step = 0.01),
numericInput(ns("FCT"),"P values threshold", min = 0, max = 10 , value = 1, step = 0.05),
DT::dataTableOutput('results_table')



)

}


#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#' @title Perform DE analysis server side
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom DESeq2 DESeqDataSetFromMatrix estimateSizeFactors sizeFactors
#' @importFrom edgeR DGEList
#' @importFrom limma contrasts.fit


DEAServer <- function(input, output, session, countmatrix = NULL, colData = NULL, Model = NULL) {


  ns <- session$ns

  ### Defin reactives #############
  reactives <-  reactiveValues(mydds = NULL)
  results <- reactiveValues(res = NULL, up = NULL, down = NULL)

observe({

  if (!is.null(countmatrix$table) && !is.null(Model$design)){

  if(input$DEAmeth == "limma"){


    y <- DGEList(counts=countmatrix$table, genes=rownames(countmatrix$table))
    #Voom transforms count data to log2-counts per million (logCPM), estimate the mean-variance relationship and use this to compute appropriate observation-level weights
    v <- voom(y, Model$design, plot=FALSE)
    fit <- lmFit(v, Model$design)
    print('design infos')
    print(head(Model$design))
    print(class(Model$design))
    print(head(Model$contrast))
    print(class(Model$contrast))
    print('end of design infos')

    fit2 <- contrasts.fit(fit,Model$contrast)
    #Given a microarray linear model fit, compute moderated t-statistics, moderated F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value.
    fit2 <- eBayes(fit2)
    print("a")
    results$res <- topTable(fit2, number=nrow(countmatrix$table), adjust.method=input$AdjMeth)
    results$res <- results$res[order(results$res$adj.P.Val),]
    hist(results$res$P.Value,main = "P values distribution :",xlab = "Pvaleurs",ylab="Occurences")
    nsign <- length(which(results$res$adj.P.Val < input$PvalsT))
    nsignfc <- length(which(results$res$adj.P.Val < input$PvalsT & abs(results$res$logFC) > FCT))
    results$up <- which(results$res$adj.P.Val < input$PvalsT & results$res$logFC > FCT)
    results$down <- which(results$res$adj.P.Val < input$PvalsT & results$res$logFC < -FCT)
    print('end of DEG')


    rownames(results$res) <- results$res$genes
    results$res$genes  <- NULL


    results$up <- results$res[results$up,]
    results$down <- results$res[results$down,]

  } else if( input$DEAmeth == "DEseq2") {

  }


  } # end of if NULL

}) # end of observer


output$results_table <- DT::renderDataTable(results$res)

    return(results)

}

