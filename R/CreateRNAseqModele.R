#' @title Check RNASeqdata for further analysis
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
              # fluidRow(column(width = 6,uiOutput(ns("variables"))),
              #          column(width = 6,uiOutput(ns("formula")))
                #)
              uiOutput(ns("var")),
              uiOutput(ns("covar")),
              uiOutput(ns("interact")),
              uiOutput(ns("formula"))
          )),
          fluidRow(box("Comparison",collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
              status = "primary",width= 12,
              uiOutput(ns("Group1")),
              uiOutput(ns("Group2")))

  ))
}


  #' @param input,output,session standards \code{shiny} server arguments.
  #' @param header Does the file have a Header
  #' @param sep What is the file separator
  #'
  #' @export
  #'
  #'
  #' @importFrom shiny renderUI modalDialog observeEvent reactiveValues callModule observe icon
  #' @importFrom htmltools tags HTML
  #' @import limma


CreateModelServer <- function(input, output, session, sampleplan = NULL , matrix = NULL ,batcheffect = NULL) {

    ns <- session$ns

    reactives <- reactiveValues(sampleplan = NULL, formula = NULL, matrix = NULL, contrast = NULL)

    reactives$sampleplan <- sampleplan
    reactives$matrix <- matrix


# observeEvent({reactives$sampleplan
#               input$var
#               input$covar
#               input$interact},{
observe({
    # if(!is.null(batcheffect)){
    #   colnames(batcheffect) <- paste0('batch',1:ncol(batcheffect))
    #   sampleplan <- data.frame(sampleplan,batcheffect)
    # }
    reactives$sampleplan <- sampleplan


    if(!is.null(sampleplan)){
      if(!is.null(input$var)){
            if(!is.null(matrix)){

    design.idx <- colnames(reactives$sampleplan)
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

observe({

  reactives$sampleplan <- sampleplan
  reactives$matrix <- matrix


  if(!is.null(sampleplan)){
    if(!is.null(input$var)){
      if(!is.null(input$Group2sel)){
        if(!is.null(input$Group1sel)){
      if(!is.null(matrix)){
        if(!is.null(reactives$formula)){

    design <- model.matrix(reactives$formula, data=reactives$sampleplan)
    rownames(design) <- colnames(reactives$matrix)
    colnames(design) <- make.names(colnames(design))
    print(paste0(input$var,input$Group1sel))
    print(paste0(input$var,input$Group2sel))
    #print(design)
    print(paste0(paste0(input$var,input$Group1sel),"-",(paste0(input$var,input$Group2sel))))
    contrast <-makeContrasts(contrasts = paste0(paste0(input$var,input$Group1sel),"-",(paste0(input$var,input$Group2sel))) ,
                              levels=design)
    reactives$contrast <- contrast
    print(reactives$contrast)
    print(class(reactives$contrast))

      }
    }
  }}}
  }
})



    output$formula <- renderUI({

       if(!is.null(sampleplan)){


         print(Reduce(paste,deparse(reactives$formula)))
       } else {
         print("Provides a sampleplan first ")
       }
     })


 output$var <- renderUI({


   selectInput(ns("var"),"Variable of interest :",choices = colnames(reactives$sampleplan),
               multiple = FALSE, selected = colnames(reactives$sampleplan)[1])

 })

 output$covar <- renderUI({
   selectInput(ns("covar"),"Covariables :",choices = c("None" ="" ,colnames(reactives$sampleplan)),
               multiple = TRUE, selectize = TRUE)


 })

 output$interact <- renderUI({
   conditionalPanel(condition = "input.covar.length > 0",
     selectInput(ns("interact"),"Compute variables interactions ?", choices = c(TRUE,FALSE),
               selected = TRUE)
   )
 })

output$Group1 <- renderUI({
                    selectInput(ns("Group1sel"),"Compute variables interactions ?", choices = unique(reactives$sampleplan[,input$var]),
                                selected= unique(reactives$sampleplan[,input$var])[1])
 })

output$Group2 <- renderUI({
                    selectInput(ns("Group2sel"),"Compute variables interactions ?", choices  = unique(reactives$sampleplan[,input$var]),
                                selected = unique(reactives$sampleplan[,input$var])[2] )

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
                           actionButton("returnToInput", "Return To Input Tab")
                         )))

}
 return(reactives$contrast)

}
