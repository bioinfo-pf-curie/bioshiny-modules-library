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
#' @importFrom shinyWidgets updatePickerInput pickerInput


MergedDeaModUI <- function(id)  {
  ns <- NS(id)
    fluidPage(
      tags$head(
        tags$style(type='text/css', ".span161 { width: 850px; }")),
      br(),
      div(class = "span161",
          tabsetPanel(type = "pills",id = "DEGtabs",
                      tabPanel("RUN DEA",
      #fluidRow(
      box(title = "Creates DEG model",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "primary",width= 12,
          #fluidRow(
          column(width = 10,uiOutput(ns("var")),
                 uiOutput(ns("covar"))),
          column(width = 2,
                 br(),
                 br(),
                 br(),
                 actionButton(ns("help1"),"",icon = icon("info")))
      ), # end of box
      #), # end of fluidRow
      #fluidRow(
      box(title = "Comparison",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "primary",width= 12,
          #fluidRow(
          column(width=6,
                 uiOutput(ns("Group1")),
                 textOutput(ns("group1table")),
                 #selectInput(ns("remove1"),"remove samples from Group 1",multiple = TRUE,selected = NULL,choices = NULL),
                 #selectInput(ns("move1"),"Move samples to Group 2",multiple = TRUE,selected = NULL,choices = NULL)),
          pickerInput(
            ns("move1"),
            label = "Move samples to Group 2",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            choicesOpt = NULL,
            inline = FALSE,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Select samples to move",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
            )
          ),
          pickerInput(
            ns("remove1"),
            label = "remove samples from Group1",
            choices = NULL,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Select samples to remove",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
            ),
            selected = NULL,
            multiple = TRUE,
            choicesOpt = NULL,
            inline = FALSE
          )),
          column(width = 6,uiOutput(ns("Group2")),
                 textOutput(ns("group2table")),
                 # selectInput(ns("remove2"),"remove samples from Group 1",multiple = TRUE,selected = NULL,choices = NULL),
                 # selectInput(ns("move2"),"Move samples to Group 2",multiple = TRUE,selected = NULL,choices = NULL))
                 pickerInput(
                   ns("move2"),
                   label = "Move samples to Group 1",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   choicesOpt = NULL,
                   inline = FALSE,
                   options = pickerOptions(
                     actionsBox = TRUE,
                     title = "Select samples to move",
                     liveSearch = TRUE,
                     liveSearchStyle = "contains"
                   )
                 ),
                 pickerInput(
                   ns("remove2"),
                   label = "remove samples from Group 2",
                   choices = NULL,
                   options = pickerOptions(
                     actionsBox = TRUE,
                     title = "Select samples to remove",
                     liveSearch = TRUE,
                     liveSearchStyle = "contains",
                   ),
                   selected = NULL,
                   multiple = TRUE,
                   choicesOpt = NULL,
                   inline = FALSE
                 ))),
      br(),
      br(),
      actionButton(ns("Build"),"Build Model")
    ), # end of first tabs

tabPanel("DEA results",
          br(),
    tagList(
      box(title = span(icon("cogs"), "Parameters"),collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "success",width= 12,
      fluidRow(
        column(width = 6,selectInput(ns("DEAmeth"),"Select a package for DEA analysis",choices = c("DEseq2","limma"), selected = "limma")),
        #column(width = 6,selectInput(ns("transformed"),"Perform DEA on ",choices = c("Raw data","Rlog Data","VST Data")))
      ),
      fluidRow(
        column(width = 6,selectInput(ns("AdjMeth"),"Select an adjustment method", choices = c("BH","none","BY","holm"), selected = "BH")),
        column(width = 6,numericInput(ns("PvalsT"),"adjusted P values threshold", min = 0, max = 1 , value = 0.05, step = 0.01))),
      fluidRow(
        column(width = 6,numericInput(ns("FCT"),"LogFC threshold", min = 0, max = 10 , value = 1, step = 0.05)),
        column(width = 6, infoBoxOutput(ns("featuress"),tags$style("#featuress {width:230px;}")))),
      ),
      #fluidRow(
        #column(width = 12 ,
          box(title = span(icon("chart-bar"),"DEA figures"),collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "success",width = 12,
          fluidRow(column(width =6,girafeOutput(ns("Pvals_distrib"))),
          column(width =6,plotOutput(ns("scatter")))),
          br(),
          br(),
          fluidRow(column(width = 12,plotOutput(ns("Volcano"))))

      #)
      #)
      ),
      box(title = span(icon("arrow-circle-down"),"Tables"),collapsible = TRUE, collapsed = TRUE,solidHeader = TRUE,
          status = "success",width= 12,
          #fluidRow(plotOutput(ns("Volcano"))),
          fluidRow(
            tags$head(tags$style(".butt{background-color:#2E8B57;}")),
            column(width =4,
                   h4("All genes :",style="padding-left:20px"),
                   br(),
                   DT::dataTableOutput(ns('results_table')),
                   downloadButton(ns("resdl"),"All genes",class = "butt")),
            #tags$br(),

            column(width = 4,
                   h4("Upp regulated genes :",style="padding-left:20px"),
                   br(),
                   DT::dataTableOutput(ns('up_table')),
                   downloadButton(ns("uppdl"),"Up-regulated",class = "butt")),
            #tags$br(),
            column(width = 4,
                   h4("Down regulated genes :",style="padding-left:20px"),
                   br(),
                   DT::dataTableOutput(ns('down_table')),
                   downloadButton(ns("downdl"),"Down-regulated",class = "butt"))
            )#,
      ) # end of box
    ) # end of Taglist
) # end of second tab
)
) # end of div
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
#' @import ggiraph
#' @import ggplot2
#' @importFrom shinydashboard renderInfoBox infoBox
#' @importFrom shiny renderUI modalDialog observeEvent reactiveValues callModule observe icon
#' @import limma
#' @importFrom ggrepel geom_text_repel
#' @importFrom shinyWidgets updatePickerInput pickerInput pickerOptions

MergedDeaModServer <- function(input, output, session, matrix = NULL,sampleplan = NULL, var = NULL) {

  ### Define reactives #############
req(sampleplan)
req(matrix)
ns <- session$ns

reactives <- reactiveValues(design = NULL, formula = NULL, contrast = NULL)
groups <- reactiveValues(Group1 = NULL, Group2 = NULL)
sampleplanmodel <- reactiveValues(table = sampleplan$table)
results <- reactiveValues(res = NULL, up = NULL, down = NULL,nsignfc = NULL,v = NULL)

observeEvent(input$remove1,{
      sampleplanmodel$table[input$remove1,input$var] <- "removed"
      #updateSelectInput(ns("var"),"Variable of interest :",selected = as.character(var), choices = colnames(sampleplan$table))
})
    observeEvent(input$move1,{
      #sampleplan$table[input$move1,input$var] <- input$Group2sel
      sampleplanmodel$table[input$move1,input$var] <- input$Group2sel
    })
    observeEvent(input$remove2,{
      #sampleplan$table[input$remove2,input$var] <- "removed"
      sampleplanmodel$table[input$remove2,input$var] <- "removed"
    })
    observeEvent(input$move2,{
      #sampleplan$table[input$move2,input$var] <- input$Group1sel
      sampleplanmodel$table[input$move2,input$var] <- input$Group1sel
    })

    observeEvent(c(input$var,
                   input$covar
    ),{
      if(!is.null(sampleplanmodel$table)){
        if(!is.null(input$var)){
          if(!is.null(matrix$table)){

            sampleplan <- sampleplanmodel$table
            print("sampleplanformula")
            print(nrow(sampleplan))
            design.idx <- colnames(sampleplan)
            if(input$covar != ""){
              print("with covar")
              vector <- c(input$var,input$covar)
              formula <- as.formula(
                paste0('~0+',input$var,"+",paste0(input$covar,collapse = "+"),"+",
                       paste0(combn(vector,2,FUN = paste,collapse =":"),collapse = "+")
                )
              )

            } else if(!is.null(input$var)){
              print("without covar")
              formula <- as.formula(
                paste0('~0+',as.character(input$var))
              )

            }
            reactives$formula <- formula
          }}}

    })


    observeEvent(input$Build,{

      if(!is.null(sampleplanmodel$table)){
        if(!is.null(input$var)){
          if(!is.null(input$Group2sel)){
            if(!is.null(input$Group1sel)){
              if(!is.null(matrix$table)){
                if(!is.null(reactives$formula)){
                  data <- sampleplanmodel$table
                  mat <- matrix$table[,rownames(data)]
                  design <- model.matrix(reactives$formula, data=data)
                  design <- design[which(rownames(design) %in% colnames(mat)), ]
                  colnames(design) <- make.names(colnames(design))
                  print("design2")
                  print(head(design))
                  print("group1sel")
                  print(input$Group1sel)
                  print("group2sel")
                  print(input$Group2sel)
                  contrast <-makeContrasts(contrasts = paste0(paste0(input$var,input$Group1sel),"-",(paste0(input$var,input$Group2sel))) ,
                                           levels=design)

                  reactives$contrast <- contrast
                  reactives$design <- design
                }
              }
            }}}
      }

    })

    output$formula <- renderUI({
      if(!is.null(sampleplan$table)){
        print(Reduce(paste,deparse(reactives$formula)))
      } else {
        print("Provides a sampleplan first ")
      }
    })

    output$var <- renderUI({

      tagList(
        selectInput(ns("var"),"Variable of interest :",choices = var,
                    multiple = FALSE, selected = var[1])
      )
    })


    observeEvent(input$help1,{

      showModal(modalDialog(HTML(
        "<b>Variable of interest :</b></br>
    The variable that you want to use to create sample groups for the DE analysis </br></br></br>

    <b>Co-variables :</b></br>

    Select co-variables if you want that app to take into account their respective effects on genes' expression.

    "),
        title = "Variables infos",
        footer = tagList(
          modalButton("Got it"),
        )))


    })

    output$covar <- renderUI({
      tagList(
        selectInput(ns("covar"),"Covariables :",choices = c("None" = "",var),
                    multiple = FALSE, selectize = TRUE, selected = "")
      )

    })

    output$Group1 <- renderUI({
      tagList(
        selectInput(ns("Group1sel"),"Group 1", choices = na.omit(levels(sampleplan$table[,input$var])),
                    selected= na.omit(levels(sampleplanmodel$table[,input$var])[1]))

      )
    })

    observe({

      groups$Group1 <- rownames(sampleplanmodel$table[which(sampleplanmodel$table[,input$var] == input$Group1sel),])
    })



    observe({
      if(!is.null(groups$Group1)){
        # updateSelectInput(session = session,
        #                   "remove1","remove samples from Group 1",selected = NULL,choices = groups$Group1)
        # updateSelectInput(session = session,
        #                   "move1","Move samples to Group 2",selected = NULL,choices = groups$Group1)
        updatePickerInput(session = session,
                          "remove1","remove samples from Group 1",selected = NULL,choices = groups$Group1,
                          pickerOptions(
                            actionsBox = TRUE,
                            title = "Select samples to remove",
                            header = "This is a title"
                          ))
        updatePickerInput(session = session,
                          "move1","Move samples to Group 2",selected = NULL,choices = groups$Group1)

      }
    })

    observe({
      if(!is.null(groups$Group2)){
        # updateSelectInput(session = session,
        #                   "remove2","remove samples from Group 2",selected = NULL,choices = groups$Group2)
        # updateSelectInput(session = session,
        #                   "move2","Move samples to Group 1 ",selected = NULL,choices = groups$Group2)
        updatePickerInput(session = session,
                          "remove2","remove samples from Group 2",selected = NULL,choices = groups$Group2,
                          pickerOptions(
                            actionsBox = TRUE,
                            title = "Select samples to remove"
                          ))
        updatePickerInput(session = session,
                          "move2","Move samples to Group 1",selected = NULL,choices = groups$Group2)

      }
    })



    output$group1table <- renderText({
      groups$Group1
    })

    output$Group2 <- renderUI({
      tagList(
        selectInput(ns("Group2sel"),"Group 2", choices  = na.omit(levels(sampleplan$table[,input$var])),
                    selected = na.omit(levels(sampleplanmodel$table[,input$var])[2]) )
      )

    })


    observe({
      groups$Group2 <- rownames(sampleplanmodel$table[which(sampleplanmodel$table[,input$var] == input$Group2sel),])
    })

    output$group2table <- renderText({
      groups$Group2
    })


    observeEvent(c(input$Group1sel,input$Group2sel),ignoreInit = TRUE,{
      if(input$Group1sel != "" & input$Group2sel != ""){
        if(input$Group1sel == input$Group2sel){
          print("group1")
          print(input$Group1sel)
          print("group2")
          print(input$Group2sel)
          showModal(modalDialog(p("You must select two different groups to make a comparison"),
                                title = "Identical groups",
                                footer = tagList(
                                  modalButton("Got it"),
                                )))
        }
      }
    })

    observeEvent({input$var
      input$covar},{

        if(input$var %in% input$covar){
          validationModalModel(
            msg = "You can not use the same sample Plan column for variable and covariable ",
          )
          return(-1)
        }

      })

    ## Function def
    validationModalModel <- function(msg = "", title = "Model Error") {
      showModal(modalDialog(p(msg),
                            title = title,
                            footer = tagList(
                              modalButton("Dismiss"),
                            )))

    }

  observeEvent(c(matrix$table,
                 reactives$contrast),priority = 10,{
                   #observe({

                   #if (!is.null(matrix$table) && !is.null(reactives$design)){
                   if (!is.null(matrix$table) && !is.null(reactives$design)){

                     #print(head(matrix$table))
                     counts <- matrix$table[,colnames(matrix$table)%in%rownames(reactives$design)]
                     if(input$DEAmeth == "limma"){

                       for (col in 1:ncol(counts)){
                         counts[,col] <- as.numeric(counts[,col])
                       }
                       y <- DGEList(counts=counts, genes=rownames(counts))

                       #Voom transforms count data to log2-counts per million (logCPM), estimate the mean-variance relationship and use this to compute appropriate observation-level weights
                       results$v <- voom(y, reactives$design, plot=FALSE, save.plot = TRUE)
                       fit <- lmFit(results$v$E, reactives$design)
                       fit2 <- contrasts.fit(fit,reactives$contrast)
                       #Given a microarray linear model fit, compute moderated t-statistics, moderated F-statistic, and log-odds of differential expression by empirical Bayes moderation of the standard errors towards a common value.
                       fit2 <- eBayes(fit2)
                       res <- topTable(fit2, number=nrow(counts), adjust.method=input$AdjMeth)
                       res <- res[order(res$adj.P.Val),]
                       rownames(res) <- res$genes
                       res$genes  <- NULL
                       #res$label <- rownames(res)
                       print(head(res))
                       results$res <- res

                     } else if( input$DEAmeth == "DEseq2") {

                     }
                   } # end of if NULL
                 }) # end of observer

  createLink <- function(val) {
    sprintf('<a href="https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=%s" target="_blank" class="btn btn-primary">Info</a>',val)
  }

  observeEvent(c(results$res,
                 input$FCT,
                 input$PvalsT),ignoreInit = TRUE,{

                   res <- results$res
                   nsign <- length(which(res$adj.P.Val < input$PvalsT))
                   results$nsignfc <- length(which(res$adj.P.Val < input$PvalsT & abs(res$logFC) > input$FCT))
                   up <- which(res$adj.P.Val < input$PvalsT & res$logFC > input$FCT)
                   down <- which(res$adj.P.Val < input$PvalsT & res$logFC < -input$FCT)
                   res$t <- NULL
                   res$P.Value <- NULL
                   res$B <- NULL
                   res$label <- NULL
                   #res$featureID <- rownames(res)
                   res$ENSEMBL <- createLink(rownames(res))
                   print('end of DEG')
                   results$up <- res[up,]
                   results$down <- res[down,]
                   results$restable <- res
                 }) # end of observer



  output$Pvals_distrib <- renderGirafe({
    req(results$res)
    plot <- ggplot(data = results$res) + aes(x = `P.Value`) +
      geom_histogram_interactive(fill = "steelblue",breaks = seq(0, 1, length.out = 20))
    build  <- ggplot_build(plot)
    plot <- plot +  labs(title = "P values distribution", x = "P values", y = "Occurences")# +
      #geom_vline_interactive(xintercept=input$PvalsT, linetype="dashed", color = "red") # +
      # annotate("text",x=input$PvalsT + 0.02, label="\nP vals Threshold", y = max(build[["data"]][[1]][["count"]])/2,
      #          colour="red", angle=90, text=element_text(size=15), vjust = 0.5)
    # geom_text(aes(x=input$PvalsT + 0.02, label="\nP vals Threshold", y = max(build[["data"]][[1]][["count"]])/2),
    #           colour="red", angle=90, text=element_text(size=15), vjust = 0.5)
    # geom text is time consuming
    ggiraph::girafe(code = {print(plot)})
  })

#output$Pvals_distrib <- renderGirafe({
output$Volcano <- renderPlot({
#output$Volcano <- renderGirafe({


  results$res$label <- rownames(results$res)
  #ggplot <- ggplot(results$res, aes(x = logFC, y = -log10(P.Value), label = adj.P.Val)) +
  ggplot <- ggplot(results$res, aes(x = logFC, y = -log10(adj.P.Val))) +
  #ggplot <- ggplot(results$res, aes(x = logFC, y = -log10(P.Value))) +
    ggtitle(colnames(reactives$contrast)) +
    scale_fill_gradient(low = "lightgray", high = "navy") +
    scale_color_gradient(low = "lightgray", high = "navy") +
    #scale_y_continuous(trans = revlog_trans(), expand = c(0.005, 0.005)) +
    expand_limits(y = c(min(-log10(results$res$P.Value)), 1)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon",
                    show.legend = FALSE) +
    geom_point(data = results$res,
               color = "grey", alpha = 0.5) +
    #geom_point(data = subset(results$res, logFC > input$FCT  & adj.P.Val > input$PvalsT),
    geom_point(data = subset(results$res, logFC > input$FCT),
               color = "red", alpha = 0.5) +
    geom_point(data = subset(results$res, logFC < -input$FCT),
               color = "blue", alpha = 0.5) +
    geom_point(data = subset(results$res, adj.P.Val < input$PvalsT),
    #geom_point(data = subset(results$res, label < input$PvalsT),
               color = "green", alpha = 0.5) +
    #geom_text_repel() +
    geom_vline(xintercept = min(-log10(results$res$P.Value))) +
    geom_hline(yintercept = min(-log10(results$res$P.Value))) +
    geom_hline(yintercept = -log10(input$PvalsT), linetype = "dashed") +
    geom_vline(xintercept = c(-input$FCT, input$FCT), linetype = "dashed") +
    theme_linedraw() +
    theme(panel.grid = element_blank()) +
    xlab("Fold change (log2)") +
    ylab("-log10(P-Value)") #+
    ggrepel::geom_text_repel(
      data = subset(results$res, adj.P.Val < input$PvalsT),
      #aes(label = results$res$label),
      aes(label = label),
      size = 5,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    )
  return(ggplot)
  #girafe(code = {print(ggplot)})
})

output$scatter <- renderPlot({

  dfplot <- data.frame(Mean = results$v$voom.xy$x, sd = results$v$voom.xy$y)
  dfline <- data.frame(x = results$v$voom.line$x,y = results$v$voom.line$y)
  ggplot <- ggplot(dfplot, aes(x= Mean,y = sd, label = rownames(dfplot))) +
    geom_point(data = dfplot,
               color = "blue", alpha = 0.05) +
    labs(title = "Scatter plot on voom normalized value",x = results$v$voom.xy$xlab,
         y = results$v$voom.xy$ylab) +
    geom_line(data = dfline,aes(x=x,y=y))

  return(ggplot)

})

  output$results_table <- DT::renderDataTable({datatable(
    results$restable,escape = FALSE,options = list(scrollX=TRUE, scrollCollapse=TRUE))})


  output$resdl <- downloadHandler(
    filename = function() {
      paste("DEA-PDX-Results", Sys.Date(), ".csv", sep=",")
    },
    content = function(file) {
      write.csv(results$res, file)
    }
  )


  output$up_table <- DT::renderDataTable({datatable(
    results$up,escape = FALSE,options = list(scrollX=TRUE, scrollCollapse=TRUE))})

  output$updl <- downloadHandler(
    filename = function() {
      paste("DEA-UPPS-PDX-Results", Sys.Date(), ".csv", sep=",")
    },
    content = function(file) {
      write.csv(results$up, file)
    }
  )


  output$down_table <- DT::renderDataTable({datatable(
    results$down,escape = FALSE,options = list(scrollX=TRUE, scrollCollapse=TRUE))})

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
