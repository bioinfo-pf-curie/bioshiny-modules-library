#' @title Draw clusters heatmap from counts matrix UI side
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
#'

ClusteringUICNV <- function(id){

  ns <- NS(id)
  ui <- shiny::shinyUI(
    shiny::fluidPage(
      tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 13px;width: 105px}
                 .selectize-dropdown { font-size: 12px; line-height: 13px; }
                 .form-group, .selectize-control {margin-left:-10px;max-height: 100px !important;}
                 .box-body {
          padding-bottom: 0px;
      }"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 5,
          htmltools::h4('Data'),
          #shiny::uiOutput(ns('data')),
          shiny::checkboxInput(ns('showSampleCNV'),'Subset Data'),
          shiny::conditionalPanel(ns('input.showSampleCNV'),shiny::uiOutput(ns('sampleCNV'))),
          # br(),
          htmltools::hr(),htmltools::h4('Data Preprocessing'),
          shiny::column(width=4,shiny::selectizeInput(ns('transposeCNV'),'Transpose',choices = c('No'=FALSE,'Yes'=TRUE),selected = FALSE)),
          shiny::column(width=4,shiny::selectizeInput(ns("transform_funCNV"), "Transform", c(Identity=".",Sqrt='sqrt',log='log',Scale='scale',Normalize='normalize',Percentize='percentize',"Missing values"='is.na10', Correlation='cor'),selected = '.')),
          shiny::uiOutput(ns('annoVarsCNV')),

          htmltools::br(),htmltools::hr(),htmltools::h4('Row dendrogram'),
          shiny::column(width=6,shiny::selectizeInput(ns("distFun_rowCNV"), "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
          shiny::column(width=6,shiny::selectizeInput(ns("hclustFun_rowCNV"), "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
          shiny::column(width=12,shiny::sliderInput(ns("rCNV"), "Number of Clusters", min = 1, max = 15, value = 2)),
          #column(width=4,numericInput("r", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),

          htmltools::br(),htmltools::hr(),htmltools::h4('Column dendrogram'),
          shiny::column(width=6,shiny::selectizeInput(ns("distFun_colCNV"), "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
          shiny::column(width=6,shiny::selectizeInput(ns("hclustFun_colCNV"), "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
          shiny::column(width=12,shiny::sliderInput(ns("cCNV"), "Number of Clusters", min = 1, max = 15, value = 2)),
          #column(width=4,numericInput("c", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),

          htmltools::br(),htmltools::hr(),  htmltools::h4('Additional Parameters'),

          shiny::column(3,shiny::checkboxInput(ns('showColorCNV'),'Color')),
          shiny::column(3,shiny::checkboxInput(ns('showMarginCNV'),'Layout')),
          shiny::column(3,shiny::checkboxInput(ns('showDendoCNV'),'Dendrogram')),
          htmltools::hr(),
          shiny::conditionalPanel(ns('input.showColorCNV==1'),ns = ns,
                                  htmltools::hr(),
                                  htmltools::h4('Color Manipulation'),
                                  shiny::uiOutput(ns('colUICNV')),
                                  shiny::sliderInput(ns("ncolCNV"), "Set Number of Colors", min = 1, max = 256, value = 256),
                                  shiny::checkboxInput(ns('colRngAutoCNV'),'Auto Color Range',value = TRUE),
                                  shiny::conditionalPanel(ns('!input.colRngAutoCNV'),shiny::uiOutput(ns('colRngCNV')))
          ),

          shiny::conditionalPanel(ns('input.showDendoCNV==1'),ns = ns,
                                  htmltools::hr(),
                                  htmltools::h4('Dendrogram Manipulation'),
                                  shiny::selectInput(ns('dendrogramCNV'),'Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both'),
                                  shiny::selectizeInput(ns("seriationCNV"), "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
                                  shiny::sliderInput(ns('branches_lwdCNV'),'Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1)
          ),

          shiny::conditionalPanel(ns('input.showMarginCNV==1'),ns = ns,
                                  htmltools::hr(),
                                  htmltools::h4('Widget Layout'),
                                  shiny::column(4,shiny::textInput(ns('mainCNV'),'Title','')),
                                  shiny::column(4,shiny::textInput(ns('xlabCNV'),'X Title','')),
                                  shiny::column(4,shiny::textInput(ns('ylabCNV'),'Y Title','')),
                                  shiny::sliderInput(ns('row_text_angleCNV'),'Row Text Angle',value = 0,min=0,max=180),
                                  shiny::sliderInput(ns('column_text_angleCNV'),'Column Text Angle',value = 45,min=0,max=180),
                                  shiny::sliderInput(ns("lCNV"), "Set Margin Width", min = 0, max = 200, value = 130),
                                  shiny::sliderInput(ns("bCNV"), "Set Margin Height", min = 0, max = 200, value = 40)
          )
          #))
        ),
        shiny::mainPanel(width = 7,
          shiny::tabsetPanel(
            shiny::tabPanel("Heatmaply",
                            htmltools::tags$a(id = 'downloadDataCNV', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, shiny::icon("clone"), 'Download Heatmap as HTML'),
                            htmltools::tags$head(htmltools::tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
                            plotly::plotlyOutput(ns("heatoutCNV"),height=paste0(500,'px'))
            ),
            shiny::tabPanel("Data",
                            shiny::dataTableOutput(ns('tablesCNV'))
            )
          )
        )
      )
    )
  )#  end of shinyUI(

}




#' @param input,output,session standards \code{shiny} server arguments.
#' @param name Does the file have a Header
#' @param data A reactiveValues containing the table reactiveValue with table a dataframe. Example data$table  a reactive dataframe
#'
#' @export
#'
#'
#' @title Draw clusters heatmap from counts matrix server side
#' @importFrom shiny observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom plotly plotlyOutput layout renderPlotly
#' @importFrom dplyr mutate_at vars
#' @import heatmaply
#' @importFrom stats cor dist hclust
#' @importFrom xtable xtable
#' @importFrom tools file_path_sans_ext
#' @importFrom rmarkdown pandoc_available pandoc_self_contained_html
#' @importFrom viridisLite viridis
#' @importFrom viridis magma plasma inferno
#' @importFrom SummarizedExperiment assay


# choices = c('Vidiris (Sequential)'="viridis",
#             'Magma (Sequential)'="magma",
#             'Plasma (Sequential)'="plasma",
#             'Inferno (Sequential)'="inferno",

#
#             'RdBu (Diverging)'="RdBu",
#             'RdYlBu (Diverging)'="RdYlBu",
#             'RdYlGn (Diverging)'="RdYlGn",
#             'BrBG (Diverging)'="BrBG",
#             'Spectral (Diverging)'="Spectral",
#
#             'BuGn (Sequential)'='BuGn',
#             'PuBuGn (Sequential)'='PuBuGn',
#             'YlOrRd (Sequential)'='YlOrRd',
#             'Heat (Sequential)'='heat.colors',
#             'Grey (Sequential)'='grey.colors'),

ClusteringServerCNV <- function(input, output, session, data = NULL, metadata = NULL,printRows = FALSE) {

  ns <- session$ns



  reactives <- reactiveValues(obj =  data$table, metadata = metadata$table)
  print("reactives$metadata in module")
  print(head(reactives$metadata))
  reactives2 <- reactiveValues(selData = data$table)
#print(reactives$metadata)


  if (!is.null(data)){


   ## End of was commented

  shiny::observeEvent(reactives2$selData,{
    output$annoVarsCNV<-shiny::renderUI({
      data.in=reactives2$selData
      NM=NULL

      if(any(sapply(data.in,class)=='factor')){
        NM=names(data.in)[which(sapply(data.in,class)=='factor')]
      }
      shiny::column(width=4,
                    #shiny::selectizeInput('annoVar','Annotation',choices = names(data.in),selected=NM,multiple=TRUE,options = list(placeholder = 'select columns',plugins = list("remove_button")))
                    shiny::selectizeInput(ns('annoVarCNV'),'Annotation',choices = colnames(reactives$metadata),selected=NM,multiple=TRUE,options = list(placeholder = 'select columns',plugins = list("remove_button")))
                    )
    })


  }) # enf of observeEvent
    #Sampling UI ----

    subdata <- reactiveValues(rows = nrow(data$table), cols = names(data$table))


    output$sampleCNV<-shiny::renderUI({
      list(
        shiny::column(4,shiny::textInput(inputId = ns('setSeedCNV'),label = 'Seed',value = sample(1:10000,1))),
        shiny::column(4,shiny::numericInput(inputId = ns('selRowsCNV'),label = 'Number of Rows',min=1,max=pmin(500,subdata$rows),value = pmin(500,subdata$rows))),
        shiny::column(4,shiny::selectizeInput(ns('selColsCNV'),'Columns Subset',choices = subdata$cols,multiple=TRUE))
      )
    })
  #}) # Intitial end of observeEvent



  output$colUICNV<-shiny::renderUI({
    colSel='Vidiris'
    if(input$transform_funCNV=='cor') colSel='RdBu'
    if(input$transform_funCNV=='is.na10') colSel='grey.colors'

    shiny::selectizeInput(inputId =ns("palCNV"), label ="Select Color Palette",
                          choices = c('Vidiris (Sequential)'="viridis",
                                      'Magma (Sequential)'="magma",
                                      'Plasma (Sequential)'="plasma",
                                      'Inferno (Sequential)'="inferno",
                                      'Magma (Sequential)'="magma",
                                      'Magma (Sequential)'="magma",

                                      'RdBu (Diverging)'="RdBu",
                                      'RdYlBu (Diverging)'="RdYlBu",
                                      'RdYlGn (Diverging)'="RdYlGn",
                                      'BrBG (Diverging)'="BrBG",
                                      'Spectral (Diverging)'="Spectral",

                                      'BuGn (Sequential)'='BuGn',
                                      'PuBuGn (Sequential)'='PuBuGn',
                                      'YlOrRd (Sequential)'='YlOrRd',
                                      'Heat (Sequential)'='heat.colors',
                                      'Grey (Sequential)'='grey.colors'),
                          selected=colSel)
  })

  shiny::observeEvent({reactives2$selData},{
    output$colRngCNV=shiny::renderUI({

      rng=range(reactives2$selData,na.rm = TRUE)

      n_data = nrow(reactives2$selData)

      min_min_range = ifelse(input$transform_funCNV=='cor',-1,-Inf)
      min_max_range = ifelse(input$transform_funCNV=='cor',1,rng[1])
      min_value = ifelse(input$transform_funCNV=='cor',-1,rng[1])

      max_min_range = ifelse(input$transform_funCNV=='cor',-1,rng[2])
      max_max_range = ifelse(input$transform_funCNV=='cor',1,Inf)
      max_value = ifelse(input$transform_funCNV=='cor',1,rng[2])

      a_good_step = 0.1 # (max_range-min_range) / n_data

      list(
        shiny::numericInput(ns("colorRng_minCNV"), "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
        shiny::numericInput(ns("colorRng_maxCNV"), "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
      )

    })
  })


  observeEvent(c(input$setSeedCNV,
                 input$selRowsCNV,
                input$selColsCNV),ignoreInit = TRUE,priority = 10, {


                   #variableFeaturesranked <- order(reactives$variableFeatures,
                    #                   decreasing=TRUE)

                   if(input$showSampleCNV){
                    data.in <- reactives$obj
                     if(!is.null(input$selRowsCNV)){
                       print("selrow not nut")
                       set.seed(input$setSeedCNV)
                       if((input$selRowsCNV >= 2) & (input$selRowsCNV < nrow(data.in))){
                         print("morethan2selrow and selrows < datain")
                         # if input$selRows == nrow(data.in) then we should not do anything (this save refreshing when clicking the subset button)
                         if(length(input$selColsCNV)<=1) {
                           print("input$selColsCNV)<=1")
                           data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRowsCNV)),]}
                           #data.in=data.in[variableFeaturesranked[1:input$selRows],]}
                         if(length(input$selColsCNV)>1) {
                           print("input$selCols)>1")
                           data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRowsCNV)),input$selColsCNV]}
                           # data.in=data.in[variableFeaturesranked[1:input$selRows],input$selCols]}
                       }
                     }
                    reactives2$selData <- data.in
                   }
    }) # end of observer
  #
  #
  #
  #
  interactiveHeatmapCNV<- shiny::reactive({
    data.in <- reactives2$selData
    print("data.in")
    print("Showsample")

    print(head(data.in))
    if(input$showSampleCNV){
      if(!is.null(input$selRowsCNV)){
        print("selrow not nut")
        set.seed(input$setSeedCNV)
        if((input$selRowsCNV >= 2) & (input$selRowsCNV < nrow(data.in))){
          print("morethan2selrow and selrows < datain")
          # if input$selRows == nrow(data.in) then we should not do anything (this save refreshing when clicking the subset button)
          if(length(input$selColsCNV)<=1) {
            print("input$selCols)<=1")
            data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRowsCNV)),]}
          if(length(input$selColsCNV)>1) {
            print("input$selCols)>1")
            data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRowsCNV)),input$selColsCNV]}
        }
      }
    }
  #
  #
    if( length(input$annoVarCNV) > 0 ){
      # print("reactives metadata")
      # print(reactives$metadata)
      samplesAnnot <- reactives$metadata[,input$annoVarCNV]

    }

  #
  #
    ss_num =  sapply(data.in, is.numeric) # in order to only transform the numeric values

    if(input$transposeCNV) data.in=t(data.in)
    if(input$transform_funCNV!='.'){
      if(input$transform_funCNV=='is.na10'){
        shiny::updateCheckboxInput(session = session,inputId = ns('showColorCNV'),value = TRUE)
        data.in[, ss_num] = heatmaply::is.na10(data.in[, ss_num])
      }
      if(input$transform_funCNV=='cor'){
        shiny::updateCheckboxInput(session = session,inputId = ns('showColorCNV'),value = TRUE)
        shiny::updateCheckboxInput(session = session,inputId = ns('colRngAutoCNV'),value = FALSE)
        data.in=stats::cor(data.in[, ss_num],use = "pairwise.complete.obs")
      }
      if(input$transform_funCNV=='log') data.in[, ss_num]= apply(data.in[, ss_num],2,log)
      if(input$transform_funCNV=='sqrt') data.in[, ss_num]= apply(data.in[, ss_num],2,sqrt)
      if(input$transform_funCNV=='normalize') data.in=heatmaply::normalize(data.in)
      if(input$transform_funCNV=='scale') data.in[, ss_num] = scale(data.in[, ss_num])
      if(input$transform_funCNV=='percentize') data.in=heatmaply::percentize(data.in)
    }


    #if(!is.null(input$tables_true_search_columns))
    #  data.in=data.in[activeRows(input$tables_true_search_columns,data.in),]
    if(input$colRngAutoCNV){
      ColLimits=NULL
    }else{
      ColLimits=c(input$colorRng_minCNV, input$colorRng_maxCNV)
    }

    distfun_row = function(x) stats::dist(x, method = input$distFun_rowCNV)
    distfun_col =  function(x) stats::dist(x, method = input$distFun_colCNV)

    hclustfun_row = function(x) stats::hclust(x, method = input$hclustFun_rowCNV)
    hclustfun_col = function(x) stats::hclust(x, method = input$hclustFun_colCNV)

    if(length(input$annoVarCNV)>0){

    p <- heatmaply::heatmaply(data.in,
                              main = input$mainCNV,xlab = input$xlabCNV,ylab = input$ylabCNV,
                              row_text_angle = input$row_text_angleCNV,
                              column_text_angle = input$column_text_angleCNV,
                              dendrogram = input$dendrogramCNV,
                              branches_lwd = input$branches_lwdCNV,
                              seriate = input$seriationCNV,
                              colors=eval(parse(text=paste0(input$palCNV,'(',input$ncolCNV,')'))),
                              distfun_row =  distfun_row,
                              hclustfun_row = hclustfun_row,
                              distfun_col = distfun_col,
                              hclustfun_col = hclustfun_col,
                              k_col = input$cCNV,
                              k_row = input$rCNV,
                              col_side_colors = samplesAnnot,
                              showticklabels = c(TRUE, printRows),
                              limits = ColLimits) %>%
      plotly::layout(margin = list(l = input$lCNV, b = input$bCNV))

    p$elementId <- NULL

    p

    } else {

      p <- heatmaply::heatmaply(data.in,
                                main = input$mainCNV,xlab = input$xlabCNV,ylab = input$ylabCNV,
                                row_text_angle = input$row_text_angleCNV,
                                column_text_angle = input$column_text_angleCNV,
                                dendrogram = input$dendrogramCNV,
                                branches_lwd = input$branches_lwdCNV,
                                seriate = input$seriationCNV,
                                colors=eval(parse(text=paste0(input$palCNV,'(',input$ncolCNV,')'))),
                                distfun_row =  distfun_row,
                                hclustfun_row = hclustfun_row,
                                distfun_col = distfun_col,
                                hclustfun_col = hclustfun_col,
                                k_col = input$cCNV,
                                k_row = input$rCNV,
                                showticklabels = c(TRUE, printRows),
                                limits = ColLimits) %>%
        plotly::layout(margin = list(l = input$lCNV, b = input$bCNV))

      p$elementId <- NULL

      p
    }

  })
  #
  #
  #
  shiny::observeEvent(reactives2$selData,{
    output$heatoutCNV <- plotly::renderPlotly({
      interactiveHeatmapCNV()
    })
  })
  #
  output$tablesCNV=shiny::renderDataTable(reactives2$selData#,server = TRUE,filter='top',
                                       #                               extensions = c('Scroller','FixedHeader','FixedColumns','Buttons','ColReorder'),
                                       #                               options = list(
                                       #                                 dom = 't',
                                       #                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),
                                       #                                 colReorder = TRUE,
                                       #                                 scrollX = TRUE,
                                       #                                 fixedColumns = TRUE,
                                       #                                 fixedHeader = TRUE,
                                       #                                 deferRender = TRUE,
                                       #                                 scrollY = 500,
                                       #                                 scroller = TRUE
                                       #                               )
  )
  #
  # #Clone Heatmap ----
  shiny::observeEvent({interactiveHeatmapCNV()},{
    h<-interactiveHeatmapCNV()

    l <- list(main = input$mainCNV,xlab = input$xlabCNV,ylab = input$ylabCNV,
            row_text_angle = input$row_text_angleCNV,
            column_text_angle = input$column_text_angleCNV,
            dendrogram = input$dendrogramCNV,
            branches_lwd = input$branches_lwdCNV,
            seriate = input$seriationCNV,
            colors=paste0(input$palCNV,'(',input$ncolCNV,')'),
            distfun_row =  input$distFun_rowCNV,
            hclustfun_row = input$hclustFun_rowCNV,
            distfun_col = input$distFun_colCNV,
            hclustfun_col = input$hclustFun_colCNV,
            k_col = input$cCNV,
            k_row = input$rCNV,
            limits = paste(c(input$colorRng_minCNV, input$colorRng_maxCNV),collapse=',')
    )


    l=data.frame(Parameter=names(l),Value=do.call('rbind',l),row.names = NULL,stringsAsFactors = FALSE)
    l[which(l$Value==''),2]='NULL'
    paramTbl=print(xtable::xtable(l),type = 'html',include.rownames=FALSE,print.results = FALSE,html.table.attributes = c('border=0'))


    h$width='100%'
    h$height='800px'
    s<-htmltools::tags$div(style="position: relative; bottom: 5px;",
                           htmltools::HTML(paramTbl),
                           htmltools::tags$em('This heatmap visualization was created using',
                                              htmltools::tags$a(href="https://github.com/yonicd/shinyHeatmaply/",target="_blank",'shinyHeatmaply'),
                                              Sys.time()
                           )
    )

    output$downloadDataCNV <- shiny::downloadHandler(
      filename = function() {
        paste0("heatmaply-", strftime(Sys.time(),'%Y%m%d_%H%M%S'), ".html")
      },
      content = function(file) {
        libdir <- paste0(tools::file_path_sans_ext(basename(file)),"_files")

        htmltools::save_html(htmltools::browsable(htmltools::tagList(h,s)),file=file,libdir = libdir)

        # if (!rmarkdown::pandoc_available()) {
        #   stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n",
        #        "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
        # }

        rmarkdown::pandoc_self_contained_html(file, file)
        unlink(libdir, recursive = TRUE)
      }
    )
  })

} # end of if !is.null(data)

return(p)

}
