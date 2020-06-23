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
#' @importFrom shinydashboard infoBoxOutput
#' @importFrom shiny NS actionButton icon uiOutput
#'
#'
#'


DEAUI <- function(id)  {
ns <- NS(id)

fluidPage(
tagList(
  fluidRow(
    column(width = 6,selectInput(ns("DEAmeth"),"Select a package for DEA analysis",choices = c("DEseq2","limma"), selected = "limma")),
    #column(width = 6,selectInput(ns("transformed"),"Perform DEA on ",choices = c("Raw data","Rlog Data","VST Data")))
    ),
  fluidRow(
    column(width = 6,selectInput(ns("AdjMeth"),"Select an adjustment method", choices = c("BH","none","BY","holm"), selected = "BH")),
    column(width = 6,numericInput(ns("PvalsT"),"adjusted P values' threshold", min = 0, max = 1 , value = 0.05, step = 0.01))),
    fluidRow(
      column(width = 6,numericInput(ns("FCT"),"LogFC threshold", min = 0, max = 10 , value = 1, step = 0.05)),
      column(width = 6, infoBoxOutput(ns("featuress"),tags$style("#featuress {width:230px;}")))),
  girafeOutput(ns("Pvals_distrib")),
  box("DEA results tables",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
               status = "success",width= 12,
      fluidRow(
               h4("All genes :",style="padding-left:20px"),
               column(width =12,DT::dataTableOutput(ns('results_table'))),
               tags$br(),
               h4("Upp regulated genes :",style="padding-left:20px"),
               column(width = 12,DT::dataTableOutput(ns('up_table'))),
               tags$br(),
               h4("Down regulated genes :",style="padding-left:20px"),
               column(width = 12,DT::dataTableOutput(ns('down_table'))),
               tags$br(),
               h4("Download tables :",style="padding-left:20px"),
               column(width = 4,downloadButton(ns("resdl"),"All genes")),
               column(width = 4,downloadButton(ns("uppdl"),"Up-regulated")),
               column(width = 4,downloadButton(ns("downdl"),"Down-regulated"))
               ) # end of Row

      ) # end of box



) # end of Taglist

) # end of FLuidPage

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
#' @import ggiraph
#' @import ggplot2
#' @importFrom shinydashboard renderInfoBox infoBox


DEAServer <- function(input, output, session, countmatrix = NULL, Model = NULL) {


ns <- session$ns
### Define reactives #############
results <- reactiveValues(res = NULL, up = NULL, down = NULL,nsignfc = NULL)

# observe({
# req(countmatrix$table)
# req(Model$contrast)

observeEvent(c(countmatrix$table,
               Model$contrast),priority = 10,{
#observe({

  #if (!is.null(countmatrix$table) && !is.null(Model$design)){
  if (!is.null(countmatrix$table) && !is.null(Model$design)){

    #print(head(countmatrix$table))
  counts <- countmatrix$table
  if(input$DEAmeth == "limma"){

    for (col in 1:ncol(counts)){
    counts[,col] <- as.numeric(counts[,col])
    }
    y <- DGEList(counts=counts, genes=rownames(counts))
    #Voom transforms count data to log2-counts per million (logCPM), estimate the mean-variance relationship and use this to compute appropriate observation-level weights
    v <- voom(y, Model$design, plot=FALSE)
    fit <- lmFit(v, Model$design)
    # print('design infos')
    # print(head(Model$design))
    # print(class(Model$design))
    # print(head(Model$contrast))
    # print(class(Model$contrast))
    # print('end of design infos')

    fit2 <- contrasts.fit(fit,Model$contrast)
    #Given a microarray linear model fit, compute moderated t-statistics, moderated F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value.
    fit2 <- eBayes(fit2)
    res <- topTable(fit2, number=nrow(counts), adjust.method=input$AdjMeth)
    res <- res[order(res$adj.P.Val),]
    rownames(res) <- res$genes
    res$genes  <- NULL

    results$res <- res

  } else if( input$DEAmeth == "DEseq2") {

  }


  } # end of if NULL

  }) # end of observer

observeEvent(c(results$res,
               input$FCT,
               input$PvalsT),ignoreInit = TRUE,{

    res <- results$res
    nsign <- length(which(res$adj.P.Val < input$PvalsT))
    results$nsignfc <- length(which(res$adj.P.Val < input$PvalsT & abs(res$logFC) > input$FCT))
    up <- which(res$adj.P.Val < input$PvalsT & res$logFC > input$FCT)
    down <- which(res$adj.P.Val < input$PvalsT & res$logFC < -input$FCT)
    print('end of DEG')


    results$up <- res[up,]
    results$down <- res[down,]


}) # end of observer



output$Pvals_distrib <- renderGirafe({

  req(results$res)

  plot <- ggplot(data = results$res) + aes(x = `P.Value`) +
    geom_histogram_interactive(fill = "steelblue")

  build  <- ggplot_build(plot)

  plot <- plot +  labs(title = "P values distribution", x = "Pvaleurs", y = "Occurences") +
    geom_vline_interactive(xintercept=input$PvalsT, linetype="dashed", color = "red") +
    annotate("text",x=input$PvalsT + 0.02, label="\nP vals Threshold", y = max(build[["data"]][[1]][["count"]])/2,
                       colour="red", angle=90, text=element_text(size=15), vjust = 0.5)
    # geom_text(aes(x=input$PvalsT + 0.02, label="\nP vals Threshold", y = max(build[["data"]][[1]][["count"]])/2),
    #           colour="red", angle=90, text=element_text(size=15), vjust = 0.5)
    # geom text is time consuming

  ggiraph::girafe(code = {print(plot)})


})

output$results_table <- DT::renderDataTable(results$res,options = list(scrollX=TRUE, scrollCollapse=TRUE))

output$resdl <- downloadHandler(
  filename = function() {
    paste("DEA-PDX-Results", Sys.Date(), ".csv", sep=",")
  },
  content = function(file) {
    write.csv(results$res, file)
  }
)


output$up_table <- DT::renderDataTable(results$up,options = list(scrollX=TRUE, scrollCollapse=TRUE))

output$updl <- downloadHandler(
  filename = function() {
    paste("DEA-UPPS-PDX-Results", Sys.Date(), ".csv", sep=",")
  },
  content = function(file) {
    write.csv(results$up, file)
  }
)


output$down_table <- DT::renderDataTable(results$down,options = list(scrollX=TRUE, scrollCollapse=TRUE))

output$downdl <- downloadHandler(
  filename = function() {
    paste("DEA-DOWN-PDX-Results", Sys.Date(), ".csv", sep=",")
  },
  content = function(file) {
    write.csv(results$down, file)
  }
)

output$featuress <-
  renderInfoBox({
    req(results$nsignfc)
    infoBox(
      "Number of features passing FC and Pval Filters",
      paste(results$nsignfc,"Among the ",nrow(results$res),"features pass the filters"),
      icon = icon("dna")
    )
})

#observeEvent(c(results$res,results$nsignfc),{
#observe({
#})
return(results)
#})

}
