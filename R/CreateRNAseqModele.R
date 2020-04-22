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
          fluidRow(box("Creates DEG model",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
                    status = "primary",width= 12,
              uiOutput(ns("var")),
              uiOutput(ns("covar")),
              uiOutput(ns("interact")),
              uiOutput(ns("formula"))
          )),
          fluidRow(box("Comparison",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
              status = "primary",width= 12,
              uiOutput(ns("Group1")),
              uiOutput(ns("Group2")),
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

observeEvent({input$var
              input$covar
              input$interact},{

    if(!is.null(sampleplan$table)){
      if(!is.null(input$var)){
            if(!is.null(matrix$table)){

    design.idx <- colnames(sampleplan$table)
    if (length(c(input$var,input$covar)) > 1){

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

    design <- model.matrix(reactives$formula, data=sampleplan$table)
    rownames(design) <- colnames(matrix$table)
    colnames(design) <- make.names(colnames(design))
    contrast <-makeContrasts(contrasts = paste0(paste0(input$var,input$Group1sel),"-",(paste0(input$var,input$Group2sel))) ,
                              levels=design)
    reactives$contrast <- contrast

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

 output$covar <- renderUI({
  tagList(
   selectInput(ns("covar"),"Covariables :",choices = c("None" ="" ,colnames(sampleplan$table)),
               multiple = TRUE, selectize = TRUE)
   )

 })

 output$interact <- renderUI({
   tagList(
     selectInput(ns("interact"),"Compute variables interactions ?", choices = c(TRUE,FALSE),
               selected = TRUE)
   )
 })

output$Group1 <- renderUI({
  tagList(
                    selectInput(ns("Group1sel"),"Group 1", choices = unique(sampleplan$table[,input$var]),
                                selected= unique(sampleplan$table[,input$var])[1])
          )
 })

output$Group2 <- renderUI({
          tagList(
                    selectInput(ns("Group2sel"),"Group 2", choices  = unique(sampleplan$table[,input$var]),
                                selected = unique(sampleplan$table[,input$var])[2] )
          )

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
