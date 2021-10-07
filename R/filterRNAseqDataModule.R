#' @title Filter counts data for further analysis UI side
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
#' @importFrom esquisse filterDF_UI
#' @importFrom shiny NS actionButton icon uiOutput
#' @importFrom shinyWidgets progressBar updateProgressBar
#' @import DT
#' @importFrom ggiraph girafeOutput


FilterRNAUI <- function(id){
  ns <- NS(id)
  tabsetPanel(id = ns("tabsetpanel"),
    tabPanel(title = "Filter TPM features",
             #id = ns("tab1"),
             #fluidRow(column(width = 9),
             #column(width = 3,actionButton(inputId = ns("tab1"),label = NULL,style = "visibility: hidden;"))),
  tagList(
    #div(id = "home",
    # numericInput(inputId = ns("TPM_sum"),
    #               label = "Select features with TPM sum >= to : ", min = 0, max = 20, value = 10),
    # numericInput(inputId = ns("TPM_mean"),
     #            label = "Select features with TPM mean >= to : ", min = 0, max = 20, value = 2),
    box(p('Filters ',actionButton(ns("startCicerone"),label=NULL,icon = icon("info-circle"))),
        id = ns('Filters'),
        collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
        status = "primary",width= 12,
        br(),
    fluidRow(column(width = 6,
    numericInput(inputId = ns("TPM_filter"),
                 label = "Select features with TPM value >= to : ", min = 0, max = 20, value = 2)),
    column(width = 6,numericInput(inputId = ns("nsamples"),
                 label = "in at least 'value' % of samples : ", min = 0, max = 20, value = 10)))),
    box(title = "Filtered table (TPM)",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
        status = "primary",width= 12,
    fluidRow(
      column(
      width = 12,
      progressBar(
        id = ns("pbar"), value = 100,
        total = 100, display_pct = TRUE
      ),
      DT::dataTableOutput(outputId = ns("table")))),
     ) # end of column
    ), # end of FluidRow
    fluidRow(
      column(
      width = 12,downloadButton(ns("dltable"), label = "Download Filtered table"))
      )
  ),
  tabPanel("Explore TPM on filtered features",# value = "tab2",
           #h1("Title", id = ns("steptitle")),
           tagList(
    #box(title = "Explore features expression",collapsible = TRUE, collapsed = TRUE,solidHeader = TRUE,
    #    status = "primary",width= 12,
    fluidRow(
      br(),
      column(
        width = 12,
        #actionButton(ns("actionButton"),"Use the feature from the table above"),
        #conditionalPanel("input.actionButton == 1",
                    pickerInput(ns("selected_genes"), label = "Select genes to explore TPM expression",
                    choices = NULL,
                    width = "100%",
                    selected = NULL,
                    multiple = TRUE,
                    choicesOpt = NULL,
                    inline = FALSE,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      title = "Select genes",
                      liveSearch = TRUE,
                      liveSearchStyle = "contains",
                    )),
                    #),
        br(),
        #plotOutput(ns("boxplots")))
        fluidRow(girafeOutput(ns("boxplots"))))
      #)
    )))# %>% add_class("tab2")
  ) # end of TagList
}

#' @param input,output,session standards \code{shiny} server arguments.
#' @param name Does the file have a Header
#' @param data A reactiveValues containing the table reactiveValue with table a dataframe. Example data$table  a reactive dataframe
#'
#' @export
#'
#'
#' @title Filter counts data for further analysiss server side
#' @importFrom shiny observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom ggiraph geom_point_interactive renderGirafe
#' @import cicerone

FilterRNAServer <- function(input, output, session, data = NULL) {

  ns <- session$ns
  
  ############# Cicerone ###########""
  guide <- Cicerone$
    new(id = ns("ciceroneGuide"))$
    step(el = ns("Filters"),
         title ="Here you can play with threshold an remove noise from your RNAseq data",
         HTML(
         "</br></br>If you observe in the <b>[DEA outlet]</b> a skewing of the p.values distribution towards 1, it indicates that the test is too conservative so results in more Type II Errors :</br></br> 
         <i>Increasing the filters' thresholds could help removing background noise</i>")
    )$
     step(
       el = ns("table"),
       title = "This table resume the features keeped after the filtering step"
    )$
      step(
        #el = ns("tab1"),
        el = "[data-value='Explore TPM on filtered features']",
        #ns("steptitle"),
        title = "You can now explore TPM expression on remaining features in the second outlet",
        is_id=FALSE
    #    #tab = ns("tab2"),
    #    #tab_id = ns("tabsetpanel")
    )
  
  observeEvent(input$startCicerone, {
    guide$init()$start()
  })
  
  # observeEvent(input$ciceroneGuide_cicerone_next, {
  #   print(input$ciceroneGuide_cicerone_next)
  #   if(grepl("tab2",input$homeGuide_cicerone_previous$before_previous) ) runjs("document.querySelector('.navbar').style.position = 'absolute'; document.querySelector('.navbar').style.position")
  #   
  # })
  ############# END of Cicerone ###########""
  
  
  rawdata <- reactiveValues(table = NULL)
  returns <- reactiveValues(DataFiltered = NULL)

  observeEvent(c(data$table,
                 input$TPM_filter,
                 input$nsamples),{
  req(input$nsamples)
  req(input$TPM_filter)

  if (!is.null(data$table)){
  data <- data$table
  isexpr <- rowSums(data >= input$TPM_filter) >= ncol(data)*input$nsamples/100
  returns$DataFiltered <-  data[isexpr,]
  } # end of if
  })

  observeEvent(returns$DataFiltered, {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(returns$DataFiltered), total = nrow(data$table),
      title = "Number of features after filtering step :"
    )
})
  
 observeEvent({input$tabsetpanel},{
   req(returns$DataFiltered)
   print(input$tabsetpanel)
   if (length(input$tabsetpanel) >= 1){
     if(input$tabsetpanel == "Explore TPM on filtered features"){
          updatePickerInput(session= session,
                            "selected_genes",choices = rownames(returns$DataFiltered))
   }}
 })
  
 output$boxplots <- renderGirafe({
   req(input$selected_genes)
   data <- returns$DataFiltered[input$selected_genes,]
   data$Genes <- rownames(data)
   data <- data %>% gather(value = "counts", key ="Samples", -Genes)
   
   p <- ggplot(data,
               aes_string(x="Genes", y="counts", fill = "Genes")) +
     geom_boxplot(outlier.alpha = FALSE)  +
     geom_point_interactive(position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.2,
                                                          seed = 1234),
                            pch=21,
                            show.legend = T,aes(tooltip = Samples))
   print(girafe(ggobj = p))
 })

    output$table <- DT::renderDataTable({
      DT::datatable(returns$DataFiltered,
      extensions = "FixedColumns",
      options = list(pageLength = 10,scrollX = TRUE,fixedColumns = list(leftColumns = 1)))
    })
    
    output$dltable <- downloadHandler(
      filename = function() {
        paste0("TPM_filtered_table",as.character(Sys.time()),".csv")
      },
      content = function(file) {
        write.table(returns$DataFiltered,file = file, sep = ",",quote = FALSE)
      })
  #end of observer
  return(returns)
}

