#' @title Check RNASeqdata for further analysis ui part
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
#' @importFrom shinydashboard box


CreateModelUI <- function(id) {

  ns <- NS(id)
  fluidPage(
          fluidRow(box(title = "Creates DEG model",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
                    status = "primary",width= 12,
              fluidRow(column(width = 10,uiOutput(ns("var")),
              uiOutput(ns("covar"))),
              column(width = 2,
                     br(),
                     br(),
                     br(),
                     actionButton(ns("help1"),"",icon = icon("info"))))
              #uiOutput(ns("interact")),
              #uiOutput(ns("formula"))
          )),
          fluidRow(box(title = "Comparison",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
              status = "primary",width= 12,
              fluidRow(
                column(width=6,
                              uiOutput(ns("Group1")),
                       textOutput(ns("group1table")),
                       selectInput(ns("remove1"),"remove samples from Group 1",multiple = TRUE,selected = NULL,choices = NULL),
                       selectInput(ns("move1"),"Move samples to Group 2",multiple = TRUE,selected = NULL,choices = NULL)),
                column(width = 6,uiOutput(ns("Group2")),
                       textOutput(ns("group2table")),
                       selectInput(ns("remove2"),"remove samples from Group 1",multiple = TRUE,selected = NULL,choices = NULL),
                       selectInput(ns("move2"),"Move samples to Group 2",multiple = TRUE,selected = NULL,choices = NULL))
              ),
              br(),
              br(),
              actionButton(ns("Build"),"Build Model"))

  ))
}


  #' @param input,output,session standards \code{shiny} server arguments.
  #' @param header Does the file have a Header
  #' @param sep What is the file separator
  #'
  #' @export
  #'
  #' @title Check RNASeqdata for further analysis server part
  #' @importFrom shiny renderUI modalDialog observeEvent reactiveValues callModule observe icon
  #' @importFrom htmltools tags HTML
  #' @import limma


CreateModelServer <- function(input, output, session, sampleplan = NULL , matrix = NULL ,batcheffect = NULL) {

req(sampleplan)
req(matrix)

ns <- session$ns
reactives <- reactiveValues(design = NULL, formula = NULL, contrast = NULL)
groups <- reactiveValues(Group1 = NULL, Group2 = NULL)

observeEvent(input$remove1,{

  sampleplan$table[input$remove1,input$var] <- "removed"

})

observeEvent(input$move1,{
  sampleplan$table[input$move1,input$var] <- input$Group2sel
})

observeEvent(input$remove2,{

  sampleplan$table[input$remove2,input$var] <- "removed"

})

observeEvent(input$move2,{
  sampleplan$table[input$move2,input$var] <- input$Group1sel
})


observeEvent(c(input$var,
              input$covar
              #input$interact
              ),{

    if(!is.null(sampleplan$table)){
      if(!is.null(input$var)){
            if(!is.null(matrix$table)){

    design.idx <- colnames(sampleplan$table)
    if (length(c(input$var,input$covar)) > 1){

    print("with covar")
    vector <- c(input$var,input$covar)

    formula <- as.formula(
        paste0('~0+',input$var,"+",paste0(input$covar,collapse = "+"),"+",
        paste0(combn(vector,2,FUN = paste,collapse =":"),collapse = "+")
        )
    )

    } else if(!is.null(input$var)){

     formula <- as.formula(
       paste0('~0+',as.character(input$var))
       )

    }
    reactives$formula <- formula
    }}}

})


observeEvent(input$Build,{


  if(!is.null(sampleplan$table)){
    if(!is.null(input$var)){
      if(!is.null(input$Group2sel)){
        if(!is.null(input$Group1sel)){
      if(!is.null(matrix())){
        if(!is.null(reactives$formula)){


    #### data, remove removed previously before calling model.matrix.
    #data <- sampleplan$table[which(sampleplan$table[,input$var] %in% c(groups$Group1,groups$Group2)),]
    data <- na.omit(sampleplan$table)
    print("data")
    print(head(data))

    design <- model.matrix(reactives$formula, data=data)
    print("design")
    print(design)
    rownames(design) <- colnames(matrix$table[,rownames(data)])
    colnames(design) <- make.names(colnames(design))
    print("design2")
    print(design)

    contrast <-makeContrasts(contrasts = paste0(paste0(input$var,input$Group1sel),"-",(paste0(input$var,input$Group2sel))) ,
                              levels=design)

    reactives$contrast <- contrast
    reactives$design <- design
    # print(reactives$contrast)
    # print(reactives$design)
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
   selectInput(ns("var"),"Variable of interest :",choices = colnames(sampleplan$table),
               multiple = FALSE, selected = colnames(sampleplan$table)[1])
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
   selectInput(ns("covar"),"Covariables :",choices = c("None" ="" ,colnames(sampleplan$table)),
               multiple = TRUE, selectize = TRUE)
   )

 })


output$Group1 <- renderUI({
  tagList(
                selectInput(ns("Group1sel"),"Group 1", choices = na.omit(levels(sampleplan$table[,input$var])),
                 selected= na.omit(levels(sampleplan$table[,input$var])[1]))

          )
 })

#observeEvent(input$Group1sel,ignoreInit = TRUE,{
#observeEvent(input$Group1sel,{
observe({
  groups$Group1 <- rownames(sampleplan$table[which(sampleplan$table[,input$var] == input$Group1sel),])
})

observe({
  if(!is.null(groups$Group1)){
  updateSelectInput(session = session,
                    "remove1","remove samples from Group 1",selected = NULL,choices = groups$Group1)
  updateSelectInput(session = session,
                    "move1","Move samples to Group 2",selected = NULL,choices = groups$Group1)
  }
})

observe({
if(!is.null(groups$Group2)){
  updateSelectInput(session = session,
                    "remove2","remove samples from Group 2",selected = NULL,choices = groups$Group2)
  updateSelectInput(session = session,
                    "move2","Move samples to Group 1 ",selected = NULL,choices = groups$Group2)
 }
})



output$group1table <- renderText({
  groups$Group1
})

output$Group2 <- renderUI({
          tagList(
                    # selectInput(ns("Group2sel"),"Group 2", choices  = na.omit(unique(sampleplan$table[,input$var])),
                    #             selected = na.omit(unique(sampleplan$table[,input$var])[2]) )
            selectInput(ns("Group2sel"),"Group 2", choices  = na.omit(levels(sampleplan$table[,input$var])),
                        selected = na.omit(levels(sampleplan$table[,input$var])[2]) )
          )

 })

#observeEvent(input$Group2sel,ignoreInit = TRUE,{
#observeEvent(input$Group2sel,{
observe({
  groups$Group2 <- rownames(sampleplan$table[which(sampleplan$table[,input$var] == input$Group2sel),])
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


return(reactives)


}
