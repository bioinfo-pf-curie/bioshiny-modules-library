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

ClusteringUI <- function(id){

  ns <- NS(id)
  #ui <- shiny::shinyUI(
    shiny::fluidPage(
      tagList(
      tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 13px;width: 105px}
                 .selectize-dropdown { font-size: 12px; line-height: 13px; }
                 .form-group, .selectize-control {margin-left:-10px;max-height: 100px !important;}
                 .box-body {
          padding-bottom: 0px;
      }"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 5,
          fluidRow(
          box(title = 'Clustering data',collapsible = TRUE,collapsed = FALSE,width = NULL, status = "primary",solidHeader = TRUE,
          #htmltools::h4('Data'),
          #shiny::uiOutput(ns('data')),
          fluidPage(
          shiny::checkboxInput(ns('showSample'),'Subset Data'),
          shiny::conditionalPanel('input.showSample == 1',ns = ns,hr(),shiny::uiOutput(ns('sample'))),
          htmltools::hr(),htmltools::h4('Data Preprocessing'),
          #shiny::column(width=4,shiny::selectizeInput(ns('transpose'),'Transpose',choices = c('No'=FALSE,'Yes'=TRUE),selected = FALSE)),
          shiny::column(width=4,shiny::selectizeInput(ns("transform_fun"), "Transform", c(Identity=".",Sqrt='sqrt',log='log',Scale='scale',Normalize='normalize',Percentize='percentize',"Missing values"='is.na10', Correlation='cor'),selected = '.')),


          shiny::uiOutput(ns('annoVars')),
          htmltools::br(),htmltools::hr(),htmltools::h4('Row dendrogram'),
          shiny::column(width=12,shiny::sliderInput(ns("r"), "Number of Clusters", min = 1, max = 15, value = 2)),
          #column(width=4,numericInput("r", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),
          htmltools::br(),htmltools::hr(),htmltools::h4('Column dendrogram'),
          shiny::column(width=12,shiny::sliderInput(ns("c"), "Number of Clusters", min = 1, max = 15, value = 2)),
          #column(width=4,numericInput("c", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),

          )
          ) # end of fluidPage
          ) # end of box and fluidRow
        ),
        #shiny::mainPanel(
        shiny::mainPanel(width = 7,
          shiny::tabsetPanel(
            shiny::tabPanel("Heatmaply",
                            htmltools::tags$a(id = 'downloadData', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, shiny::icon("clone"), 'Download Heatmap as HTML'),
                            htmltools::tags$head(htmltools::tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
                            #plotly::plotlyOutput(ns("heatout"),height=paste0(500,'px')),
                            #column(width = 12,plotly::plotlyOutput(ns("heatout"),height="100%",width = "700px")),
                            fluidRow(
                              column(width = 12,plotly::plotlyOutput(ns("heatout"),height="100%",width = "100%"))),
                            column(width = 12,
                                   br(),
                                   br(),
                                   fluidRow(
                                     box(title = "Additionnal Parameters", collapsible = TRUE,
                                         collapsed = TRUE, status = "primary", width = NULL, solidHeader = TRUE,
                                         fluidPage(
                                           #htmltools::br(),htmltools::hr(),
                                           #htmltools::h4('Additional Parameters'),
                                           shiny::column(3,shiny::checkboxInput(ns('showColor'),'Color')),
                                           shiny::column(3,shiny::checkboxInput(ns('showMargin'),'Layout')),
                                           shiny::column(3,shiny::checkboxInput(ns('showDendo'),'Dendrogram')),
                                           htmltools::hr(),
                                           shiny::conditionalPanel('input.showColor==1',ns = ns,
                                                                   htmltools::hr(),
                                                                   htmltools::h4('Color Manipulation'),
                                                                   shiny::uiOutput(ns('colUI')),
                                                                   shiny::sliderInput(ns("ncol"), "Set Number of Colors", min = 1, max = 256, value = 256),
                                                                   shiny::checkboxInput(ns('colRngAuto'),'Auto Color Range',value = TRUE),
                                                                   shiny::conditionalPanel('!input.colRngAuto',ns=ns,shiny::uiOutput(ns('colRng')))
                                           ),
                                           shiny::conditionalPanel('input.showDendo==1',ns = ns,
                                                                   htmltools::hr(),
                                                                   htmltools::h4('Dendrogram Manipulation'),
                                                                   shiny::column(width=12,shiny::selectInput(ns('dendrogram'),'Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both')),
                                                                   #htmltools::br(),htmltools::hr(),
                                                                   htmltools::h4('Row dendrogram'),
                                                                   #shiny::column(width=6,shiny::selectizeInput(ns("distFun_row"), "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
                                                                   shiny::column(width=6,shiny::selectizeInput(ns("distFun_row"), "Distance method", c(Euclidean="euclidean"),selected = 'euclidean')),
                                                                   shiny::column(width=6,shiny::selectizeInput(ns("hclustFun_row"), "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
                                                                   htmltools::br(),htmltools::hr(),htmltools::h4('Column dendrogram'),
                                                                   #shiny::column(width=6,shiny::selectizeInput(ns("distFun_col"), "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
                                                                   shiny::column(width=6,shiny::selectizeInput(ns("distFun_col"), "Distance method", c(Euclidean="euclidean"),selected = 'euclidean')),
                                                                   shiny::column(width=6,shiny::selectizeInput(ns("hclustFun_col"), "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
                                                                   br(),
                                                                   shiny::column(width=12,
                                                                                 shiny::selectizeInput(ns("seriation"), "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
                                                                                 shiny::sliderInput(ns('branches_lwd'),'Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1))
                                           ),
                                           shiny::conditionalPanel('input.showMargin==1',ns = ns,
                                                                   htmltools::hr(),
                                                                   htmltools::h4('Widget Layout'),
                                                                   shiny::column(4,shiny::textInput(ns('main'),'Title','')),
                                                                   shiny::column(4,shiny::textInput(ns('xlab'),'X Title','')),
                                                                   shiny::column(4,shiny::textInput(ns('ylab'),'Y Title','')),
                                                                   shiny::sliderInput(ns('row_text_angle'),'Row Text Angle',value = 0,min=0,max=180),
                                                                   br(),
                                                                   shiny::sliderInput(ns('column_text_angle'),'Column Text Angle',value = 45,min=0,max=180),
                                                                   shiny::sliderInput(ns("l"), "Set Margin Width", min = 0, max = 200, value = 5),
                                                                   shiny::sliderInput(ns("b"), "Set Margin Height", min = 0, max = 200, value = 5)
                                            )

                                         )# end of FluidPage
                                     ) # end of Box
                                   ) # end of fluidRow
                            ) # end of columnbox
            ),
            shiny::tabPanel("Data",
                            fluidRow(shiny::dataTableOutput(ns('tables')))
            )
          )
        ) # end of box
 # )#  end of shinyUI(
) # end of sidebarlayout
) # end of tagList
) # end of fluidPage
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

ClusteringServer <- function(input, output, session, data = NULL, metadata = NULL,printRows = FALSE, vst = FALSE) {

  ns <- session$ns

  reactives <- reactiveValues(obj =  data$table, metadata = metadata$table,variableFeatures = genefilter::rowVars(vst))
  reactives2 <- reactiveValues(selData = data$table)

  if (!is.null(data)){

  shiny::observeEvent(reactives2$selData,{
    output$annoVars<-shiny::renderUI({
      data.in=reactives2$selData
      NM=NULL

      if(any(sapply(data.in,class)=='factor')){
        NM=names(data.in)[which(sapply(data.in,class)=='factor')]
      }
      shiny::column(width=4,
                    #shiny::selectizeInput('annoVar','Annotation',choices = names(data.in),selected=NM,multiple=TRUE,options = list(placeholder = 'select columns',plugins = list("remove_button")))
                    shiny::selectizeInput(ns('annoVar'),'Annotation',choices = colnames(reactives$metadata),selected=NM,multiple=TRUE,options = list(placeholder = 'select columns',plugins = list("remove_button")))
                    )
    })


  }) # enf of observeEvent
    #Sampling UI ----

    subdata <- reactiveValues(rows = nrow(data$table), cols = names(data$table))


    output$sample<-shiny::renderUI({
      list(
        #shiny::column(4,shiny::textInput(inputId = ns('setSeed'),label = 'Seed',value = sample(1:10000,1))),
        shiny::column(4,shiny::numericInput(inputId = ns('selRows'),label = 'Number of Rows',min=1,max=pmin(500,subdata$rows),value = pmin(500,subdata$rows))),
        shiny::column(4,shiny::selectizeInput(ns('selCols'),'Columns Subset',choices = subdata$cols,multiple=TRUE))
      )
    })
  #}) # Intitial end of observeEvent



  output$colUI<-shiny::renderUI({
    colSel='Vidiris'
    if(input$transform_fun=='cor') colSel='RdBu'
    if(input$transform_fun=='is.na10') colSel='grey.colors'

    shiny::selectizeInput(inputId =ns("pal"), label ="Select Color Palette",
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
    output$colRng=shiny::renderUI({

      rng=range(reactives2$selData,na.rm = TRUE)

      n_data = nrow(reactives2$selData)

      min_min_range = ifelse(input$transform_fun=='cor',-1,-Inf)
      min_max_range = ifelse(input$transform_fun=='cor',1,rng[1])
      min_value = ifelse(input$transform_fun=='cor',-1,rng[1])

      max_min_range = ifelse(input$transform_fun=='cor',-1,rng[2])
      max_max_range = ifelse(input$transform_fun=='cor',1,Inf)
      max_value = ifelse(input$transform_fun=='cor',1,rng[2])

      a_good_step = 0.1 # (max_range-min_range) / n_data

      list(
        shiny::numericInput(ns("colorRng_min"), "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
        shiny::numericInput(ns("colorRng_max"), "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
      )

    })
  })


  observeEvent(c(#input$setSeed,
                 input$selRows,
                 input$selCols),ignoreInit = TRUE,priority = 10, {


                   variableFeaturesranked <- order(reactives$variableFeatures,
                                       decreasing=TRUE)

                   if(input$showSample){
                    data.in <- reactives$obj
                     if(!is.null(input$selRows)){
                       # print("selrow not nut")
                       # set.seed(input$setSeed)
                       set.seed(500)
                       if((input$selRows >= 2) & (input$selRows < nrow(data.in))){
                         print("morethan2selrow and selrows < datain")
                         # if input$selRows == nrow(data.in) then we should not do anything (this save refreshing when clicking the subset button)
                         if(length(input$selCols)<=1) {
                           print("input$selCols)<=1")
                           data.in=data.in[variableFeaturesranked[1:input$selRows],]}
                         if(length(input$selCols)>1) {
                           print("input$selCols)>1")
                           data.in=data.in[variableFeaturesranked[1:input$selRows],input$selCols]}
                       }
                     }
                    reactives2$selData <- data.in
                   }
    }) # end of observer


interactiveHeatmap<- shiny::reactive({
    data.in <- reactives2$selData
    # if(input$showSample){
      # if(!is.null(input$selRows)){
      #   print("selrow not nut")
      #   #set.seed(input$setSeed)
      #   set.seed(500)
      #   if((input$selRows >= 2) & (input$selRows < nrow(data.in))){
      #     print("morethan2selrow and selrows < datain")
      #     # if input$selRows == nrow(data.in) then we should not do anything (this save refreshing when clicking the subset button)
      #     if(length(input$selCols)<=1) {
      #       print("input$selCols)<=1")
      #       data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRows)),]}
      #     if(length(input$selCols)>1) {
      #       print("input$selCols)>1")
      #       data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRows)),input$selCols]}
      #   }
      # }
    #}


    if( length(input$annoVar) > 0 ){

      samplesAnnot <- reactives$metadata[,input$annoVar, drop = F]

    }
    ss_num =  sapply(data.in, is.numeric) # in order to only transform the numeric values
    #if(input$transpose) data.in=t(data.in)
    if(input$transform_fun!='.'){
      if(input$transform_fun=='is.na10'){
        shiny::updateCheckboxInput(session = session,inputId = ns('showColor'),value = TRUE)
        data.in[, ss_num] = heatmaply::is.na10(data.in[, ss_num])
      }
      if(input$transform_fun=='cor'){
        shiny::updateCheckboxInput(session = session,inputId = ns('showColor'),value = TRUE)
        shiny::updateCheckboxInput(session = session,inputId = ns('colRngAuto'),value = FALSE)
        data.in=stats::cor(data.in[, ss_num],use = "pairwise.complete.obs")
      }
      if(input$transform_fun=='log') data.in[, ss_num]= apply(data.in[, ss_num],2,log)
      if(input$transform_fun=='sqrt') data.in[, ss_num]= apply(data.in[, ss_num],2,sqrt)
      if(input$transform_fun=='normalize') data.in=heatmaply::normalize(data.in)
      if(input$transform_fun=='scale') data.in[, ss_num] = scale(data.in[, ss_num])
      if(input$transform_fun=='percentize') data.in=heatmaply::percentize(data.in)
    }

    if(input$colRngAuto){
      ColLimits=NULL
    }else{
      ColLimits=c(input$colorRng_min, input$colorRng_max)
    }

    #distfun_row = function(x) stats::dist(x, method = "euclidean")
    if (length(input$distFun_row) != 0){
    if(input$distFun_row != "pearson"){
    distfun_row = function(x) stats::dist(x, method = input$distFun_row)
    } else {
    distfun_row <- function(x) {
      print(head(x))
      return(1- factoextra::get_dist(x, method = "pearson", stand = FALSE))}
    }
    }
    if (length(input$distFun_col) != 0){
      print(input$distFun_col)
      if(input$distFun_col != "pearson"){
        distfun_col = function(x) stats::dist(x, method = input$distFun_col)
      } else {
        distfun_col <- function(x) {
          print(head(x))
          return(1- factoextra::get_dist(x, method = "pearson", stand = FALSE))}
      }
    }
    # distfun_row =  function(x) stats::dist(x, method = input$distFun_row)
    # distfun_col =  function(x) stats::dist(x, method = input$distFun_col)

    hclustfun_row = function(x) stats::hclust(x, method = input$hclustFun_row)
    hclustfun_col = function(x) stats::hclust(x, method = input$hclustFun_col)

    if(length(input$annoVar)>0){

    # if(length(input$annoVar) == 1){
    # Annot_name <- input$annoVar
    p <- heatmaply::heatmaply(data.in,
                              main = input$main,xlab = input$xlab,ylab = input$ylab,
                              row_text_angle = input$row_text_angle,
                              column_text_angle = input$column_text_angle,
                              dendrogram = input$dendrogram,
                              branches_lwd = input$branches_lwd,
                              seriate = input$seriation,
                              colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
                              distfun_row =  distfun_row,
                              hclustfun_row = hclustfun_row,
                              distfun_col = distfun_col,
                              hclustfun_col = hclustfun_col,
                              k_col = input$c,
                              k_row = input$r,
                              col_side_colors = samplesAnnot,
                              showticklabels = c(TRUE, printRows),
                              limits = ColLimits) %>%
      plotly::layout(margin = list(l = input$l, b = input$b))

    p$elementId <- NULL

    p

    } else {

      p <- heatmaply::heatmaply(data.in,
                                main = input$main,xlab = input$xlab,ylab = input$ylab,
                                row_text_angle = input$row_text_angle,
                                column_text_angle = input$column_text_angle,
                                dendrogram = input$dendrogram,
                                branches_lwd = input$branches_lwd,
                                seriate = input$seriation,
                                colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
                                distfun_row =  distfun_row,
                                hclustfun_row = hclustfun_row,
                                distfun_col = distfun_col,
                                hclustfun_col = hclustfun_col,
                                k_col = input$c,
                                k_row = input$r,
                                showticklabels = c(TRUE, printRows),
                                limits = ColLimits) %>%
        plotly::layout(margin = list(l = input$l, b = input$b))



        if (length(input$distFun_row) != 0){
          if(input$distFun_row != "pearson"){
            p$dendRow <- cutree(
            hclust(dist(data.in,method = input$distFun_row),method = input$hclustFun_row),
            k = input$r)
          } else {
            p$dendRow <- cutree(
            hclust(1- factoextra::get_dist(data.in, method = "pearson", stand = FALSE),
                   method = input$hclustFun_row),
            k = input$r)
          }
        }
      # p$dendRow <- cutree(
      #   #hclust(dist(data.in,method = input$distFun_row),method = input$hclustFun_row),
      #   k = input$r
      #)
      # print("rowclust")
      # print(p$dendRow)

        if (length(input$distFun_col) != 0){
          if(input$distFun_col != "pearson"){
            p$dendCol <- cutree(
            hclust(dist(data.in,method = input$distFun_col),method = input$hclustFun_col),
            k = input$c)
          } else {
            p$dendCol <- cutree(
            hclust(1- factoextra::get_dist(data.in, method = "pearson", stand = FALSE),
                   method = input$hclustFun_col),
            k = input$c)
          }
        }
      #p$dendCol <- cutree(
        #hclust(dist(t(data.in),method = input$distFun_col),method = input$hclustFun_col),
       # k = input$c)

      p$elementId <- NULL
      p
    }
  })

  shiny::observeEvent(reactives2$selData,{
    output$heatout <- plotly::renderPlotly({
      interactiveHeatmap()
    })
  })

  output$tables=shiny::renderDataTable(reactives2$selData#,server = TRUE,filter='top',
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

  #Clone Heatmap ----
  shiny::observeEvent({interactiveHeatmap()},{
    h<-interactiveHeatmap()

    l<-list(main = input$main,xlab = input$xlab,ylab = input$ylab,
            row_text_angle = input$row_text_angle,
            column_text_angle = input$column_text_angle,
            dendrogram = input$dendrogram,
            branches_lwd = input$branches_lwd,
            seriate = input$seriation,
            colors=paste0(input$pal,'(',input$ncol,')'),
            distfun_row =  input$distFun_row,
            hclustfun_row = input$hclustFun_row,
            distfun_col = input$distFun_col,
            hclustfun_col = input$hclustFun_col,
            k_col = input$c,
            k_row = input$r,
            limits = paste(c(input$colorRng_min, input$colorRng_max),collapse=',')
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

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0("heatmaply-", strftime(Sys.time(),'%Y%m%d_%H%M%S'), ".html")
      },
      content = function(file) {
        libdir <- paste0(tools::file_path_sans_ext(basename(file)),"_files")

        htmltools::save_html(htmltools::browsable(htmltools::tagList(h,s)),file=file,libdir = libdir)

        if (!rmarkdown::pandoc_available()) {
          stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n",
               "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
        }

        rmarkdown::pandoc_self_contained_html(file, file)
        unlink(libdir, recursive = TRUE)
      }
    )
  })

} # end of if !is.null(data)

#print(return)
#print(names(p))
#print(names(interactiveHeatmap()))
#return(p)
return(interactiveHeatmap())

}
