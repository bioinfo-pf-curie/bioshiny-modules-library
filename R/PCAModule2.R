#' @title Module for drawing PCA from counts matrix ui part
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
#' @importFrom shiny NS actionButton icon
#' @import pcaMethods


DrawPCAUI2 <- function(id) {
  ns <- NS(id)
  fluidPage(
  #fillPage(
   tags$head(
      tags$style(type='text/css', ".span12 { width: 510px; }"),
      tags$style(type='text/css', ".span17 { width: 510px; height = 2000px }")
    ),
    tagList(
    #div(class = "span17",
    tabsetPanel(
      tabPanel("Parameters",
               fluidPage(
                # fillPage(
                   fluidRow(#actionButton(ns("checkdata"),"Check Data"),
                   #fillRow(
                                    column(4,
                                         p("Select options for the PCA (we are using the ", a("prcomp", href = "http://stat.ethz.ch/R-manual/R-patched/library/stats/html/prcomp.html"), "function):"),
                                         wellPanel(
                                           # NOTE: this is placed on this tab, otherwise each time the slider is moved
                                           # just a little bit, it will cause the plots to recalculate. We can get around
                                           # this if we change to a different type of control
                                           uiOutput(ns('selectNumGenes')),
                                           #tagList(
                                           checkboxInput(inputId = ns('center'),
                                                         label = 'Shift variables to be zero-centered',
                                                         value = TRUE),
                                           checkboxInput(inputId = ns('scale_data'),
                                                         label = 'Scale variables to have unit variance',
                                                         value = TRUE),
                                           radioButtons(ns('normalization'), 'Normalization',
                                                        choices = c('None'='NONE',
                                                                    'Variance Stabilizing Transform (vst)'='vst',
                                                                    'Regularized logarithm (rlog) - WARNING: this can take considerable time'='rlog'),
                                                        selected = 'NONE')
                                           #)#end of TagList
                                           ) # end wellPanel
               ),column(6,
                        wellPanel(
                          fluidRow(uiOutput(ns("choose_samples_pca")))
                        )
               )),
               fluidRow(actionButton(ns("pcago"),"RunPCA")))
               #fillRow(actionButton(ns("pcago"),"RunPCA")))

               #)#end of Taglist
      ), # end  tab

      tabPanel("Plots",
               h3("Scree plot"),
               p("The scree plot shows the variances of each PC, and the cumulative variance explained by each successive PC (in %) "),
               fluidRow(
               #fillRow(
                column(8,
                               plotOutput(ns("SCREE_PLOT"), height = "300px")

               ),
               column(4,
                      wellPanel(uiOutput(ns("pc_range")))
               )
               ),
               fluidRow(
               #fillRow(
                 h3("PC plot: zoom and select points"),
                 p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                 p("Then select points on zoomed plot below to get more information about the points."),
                 p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update.")),
               fluidRow(
               #fillRow(
                 column(8,
                               plotOutput (ns("PCA_PLOT"), width = '100%',
                                           brush = brushOpts(
                                             id = "PCA_PLOTBrush",
                                             resetOnNew = TRUE))
               ),column(4,
                        wellPanel(
                          uiOutput(ns("the_grouping_variable")),
                          uiOutput(ns("the_pcs_to_plot_x")),
                          uiOutput(ns("the_pcs_to_plot_y")),
                          checkboxInput(inputId = ns('draw_ellipse'),
                                        label = 'Draw ellipse around groups',
                                        value = TRUE),
                          checkboxInput(inputId = ns('label_points'),
                                        label = 'Use sample labels for data points',
                                        value = FALSE),
                          checkboxInput(inputId = ns('select_display'),
                                        label = 'Show list of samples to display on plot',
                                        value = FALSE),
                          conditionalPanel(condition="input.select_display==true",
                                           p("Deselecting samples from the list below will remove them from the plot without recalculating the PCA.
                        To remove samples from the PCA calculation, deselect them from the 'Parameters' tab"),
                                           uiOutput(ns("choose_samples_display"))

                          #) #End of TagList
                                           )
                        )), # end row
               h3("Zoomed biplot"),
               p("The selected points in the plots above are zoomed in on this plot and their details are available in the table below."),
               column(8,
                      plotOutput(ns("ZOOMED_PLOT"), height = 400)
               ),
               h3("Zoomed points table"),
               p("Details of the points displayed in the zoomed plot above:"),
               tableOutput(ns("brush_info_after_zoom")))
      ), # end  tab

      tabPanel("Output",
               #fillPage(
               downloadLink(ns("downloadPCAOutput"), "Download PCA output (sample coefficients)"),
               br(),
               downloadLink(ns("downloadPCARotation"), "Download PCA rotation (gene loadings)"),
               p("Output of the PCA function:"),
               fluidRow(verbatimTextOutput(ns("pca_details")))#)
      ), # end  tab
      id="mainTabPanel",
      selected="Parameters"
    #)# end of div
    ) # end tabsetPanel
    ) # end fluidPage
    #) # enfd of fill

) # end tagList
}



#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#' @title Module for drawing PCA from counts matrix server part
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @import pcaMethods
#' @import ggplot2
#' @importFrom matrixStats rowVars

DrawPCAServer2 <- function(input, output, session, matrix = NULL, annotation = NULL, metadata = NULL) {

  ns <- session$ns
  req(matrix)
  req(metadata)
  reactives <- reactiveValues(combined = NULL, matrix = NULL, metadata = NULL)
  # read in the CSV
  # this is reactive and should only change if the CSV file is changed
  observeEvent( matrix$table, {

    if (!is.null(matrix$table)){
      matrix <- as.data.frame(t(matrix$table))
      matrix <- matrix[order(row.names(matrix)), ]
      reactives$matrix <- matrix
    }


  })

  observeEvent( metadata$table,{

    if (!is.null(metadata$table)){

      cnames <- colnames(metadata$table)
      num_cols <- ncol(metadata$table)
      the_metadata_plus <- data.frame(metadata$table, metadata$table[,1])
      # sort the colData by row names for good measure
      the_metadata_plus <- the_metadata_plus[order(row.names(the_metadata_plus)), ]
      # now remove the last column
      the_metadata_plus <- the_metadata_plus[1:num_cols]

      # make each column a factor
      the_metadata_plus[1:length(the_metadata_plus)] <-
        as.data.frame(lapply(the_metadata_plus, factor))

      reactives$metadata <- the_metadata_plus


    }


  })



  # combine the data & metadata for PCA visualization
  # and validate that data are good

  observeEvent({
    c(reactives$metadata,
    reactives$matrix)}, {

      if ((!is.null(reactives$metadata)) & (!is.null(reactives$matrix)))  {
        # check that all samples from the count data are present in the metadata and vice versa
        #print(rownames(reactives$metadata))
        #print(rownames(reactives$matrix))
        # now combine them according to the row / column names
        combined <- merge(reactives$matrix, reactives$metadata, by = "row.names")
        # assign the row names and remove the row.names column
        rownames(combined) <- combined$Row.names
        combined <- combined[-1]
        # sort by row names
        combined <- combined[order(row.names(combined)), ]

        reactives$combined <- combined
      }#fin du if
    })




  # display a table of the CSV contents
  output$contents <-  DT::renderDataTable({
    #
    reactives$matrix
  })

  # display a summary of the CSV contents
  output$summary <-  renderTable({
    psych::describe(reactives$matrix)
    cat(file = stderr(), "past describe")
  })

  output$selectNumGenes <- renderUI({

    # max_genes = length(matrix$table[1,])
    #print(length(reactives$combined[1,]))
    #tagList(
    sliderInput(ns('num_top_genes'), 'Number of genes to use for calculating PCA (sorted by variance)',
                min=10,
                max=length(reactives$combined[1,]),
                step=100,
                value=min(length(reactives$combined[1,])))
   #)


  })


  # Check boxes to choose samples to display on the plot
  output$choose_samples_display <- renderUI({
    the_metadata <- pca_objects$the_metadata

    samplenames <- rownames(the_metadata)

    # Create the checkboxes and select them all by default
    #tagList(
    checkboxGroupInput(ns("display_samples"),
                       "Choose samples to display on the plot:",
                       choices  = samplenames,
                       selected = samplenames)
    #)
     })


  #### Il fau que les samplenames soient dipi avant de lancer PCAGo sinan ça marche pas !!!
  # Check boxes to choose columns

  output$choose_samples_pca <- renderUI({


    # Create the checkboxes and select them all by default
    #tagList(
    checkboxGroupInput(ns("samples"),
                       "Choose samples to include in the PCA calculation:",
                       choices  = rownames(reactives$combined),
                       selected = rownames(reactives$combined))
    #)
  })

  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    the_metadata <- reactives$metadata

    the_data_group_cols <- names(the_metadata)

    #tagList(
    # drop down selection
    selectInput(
      inputId = ns("the_grouping_variable"),
      label = "Color by:",
      choices = c("None", the_data_group_cols))
    #) #end of TagList

  })


  pca_objects <- reactiveValues(pca_output = NULL,
                                pcs_df = NULL ,
                                the_metadata = NULL)

  # run the PCA an create the necessary data frames
  #observe
  #observeEvent( reactives$combined,{
  observeEvent( input$pcago , {


    if (!(is.null(reactives$matrix) | is.null(reactives$metadata))){


      withProgress(message = 'PCA calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     the_data <- na.omit(reactives$matrix)
                     incProgress(0.1)
                     the_metadata <- reactives$metadata
                     incProgress(0.1)
                     all_the_data <- reactives$combined
                     incProgress(0.1)


                     # Keep the selected samples
                     samples <- input$samples
                     # if the samples have not been selected, use all
                     if (is.null(samples)) {
                       samples <- rownames(the_data)
                     }
                     # TODO: move this into 'the_data_fn' or somehow allow for normalization to not have to be recalculated each time any of the PCA
                     #       parameters is changed...although maybe it should?

                     # subselect the samples

                     the_data_subset <-
                       the_data[which(rownames(the_data) %in% samples),]
                     incProgress(0.1)

                     # remove columns with 0 variance:

                     the_data_subset <-
                       the_data_subset[, !apply(the_data_subset, MARGIN = 2, function(x)
                         max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
                     incProgress(0.1)

                     #print(colnames(the_data_subset)[1:3])

                     # subselect the genes (either given as a list or as the topX most variable)
                     rv <- rowVars(t(the_data_subset))
                     ntop = input$num_top_genes
                     if (ntop > length(rv)) {
                       ntop = length(rv)
                     }
                     select_genes <- order(rv, decreasing = TRUE)[seq_len(ntop)]
                     the_data_subset <- the_data_subset[,select_genes]

                     # normalize, if we were requested to do so
                     normalization <- input$normalization
                     if (normalization == 'rlog') {
                       print("proceeding with rlog transformation")
                       library(DESeq2)
                       # for rlog & vst we first need to transform the data
                       transformed_subset <- as.matrix(t(round(the_data_subset)))
                       transformed_rlog_subset <- rlog(transformed_subset)
                       the_data_subset <- t(transformed_rlog_subset)
                     } else if (normalization == 'vst') {
                       print("proceeding with vst transformation")
                       library(DESeq2)
                       # for rlog & vst we first need to transform the data
                       transformed_subset <- as.matrix(t(round(the_data_subset)))
                       transformed_vst_subset <- vst(transformed_subset)
                       the_data_subset <- t(transformed_vst_subset)
                     } else if (is.null(normalization) | normalization == 'NONE') {
                       print("no normalization requested")
                     } else {
                       print(paste(
                         "Unrecognized normalization type: ",
                         normalization,
                         sep = ""
                       ))
                     }
                     incProgress(0.2)

                     the_metadata_subset <-
                       the_metadata[which(rownames(the_metadata) %in% rownames(the_data_subset)),]
                     all_the_data_subset <-
                       all_the_data[which(rownames(all_the_data) %in% rownames(the_data_subset)),]
                     incProgress(0.1)

                     # from http://rpubs.com/sinhrks/plot_pca
                     pca_output <- prcomp(
                       na.omit(the_data_subset),
                       center = input$center,
                       scale = input$scale_data
                     )
                     incProgress(0.1)

                     # data.frame of PCs
                     pcs_df <- cbind(all_the_data_subset, pca_output$x)
                     incProgress(0.1)
                   }) # end of withProgress

      pca_objects$pca_output <- pca_output
      pca_objects$pcs_df <- pcs_df
      pca_objects$the_metadata <- the_metadata_subset

    }
  })
  #
  #   # output a numeric control with the range of the PCs
  #   # for selecting in the scree plot
  output$pc_range <- renderUI({
    pca_output <- pca_objects$pca_output$x
    #tagList(
    numericInput(ns("pc_range"),
                 "Number of PCs to plot",
                 value=10,
                 min = 1,
                 max = length(pca_output[,1]),
                 width= '100px')
    #)#End of TagList

  })

  output$the_pcs_to_plot_x <- renderUI({
    #pca_output <- pca_objects$pca_output$x

    # drop down selection
    #tagList(
    selectInput(
      inputId = ns("the_pcs_to_plot_x"),
      label = "X axis:",
      choices = colnames(pca_objects$pca_output$x),
      selected = colnames(pca_objects$pca_output$x)[1])
    #)#End of TagList
  })

  output$the_pcs_to_plot_y <- renderUI({

    # drop down selection
    #tagList(
    selectInput(
      inputId = ns("the_pcs_to_plot_y"),
      label = "Y axis:",
      choices = colnames(pca_objects$pca_output$x),
      selected = colnames(pca_objects$pca_output$x)[2])
    #) #End of TagList
  })

    output$SCREE_PLOT <- renderPlot({
      pca_output <- pca_objects$pca_output
      eig = (pca_output$sdev) ^ 2
      variance <- eig * 100 / sum(eig)
      cumvar <- paste(round(cumsum(variance), 1), "%")
      eig_df <- data.frame(eig = eig,
                           PCs = colnames(pca_output$x),
                           cumvar =  cumvar)

      num_PCS_to_plot = input$pc_range

      # limit to 10 PCs
      eig_df <- eig_df[1:num_PCS_to_plot,]
      eig <- eig[1:num_PCS_to_plot]
      cumvar <- cumvar[1:num_PCS_to_plot]

      ggplot(eig_df, aes(reorder(PCs,-eig), eig)) +
        geom_bar(stat = "identity",
                 fill = "white",
                 colour = "black") +
        geom_text(label = cumvar,
                  size = 4,
                  vjust = -0.4) +
        theme_bw(base_size = 14) +
        xlab("PC") +
        ylab("Variances") +
        ylim(0, (max(eig_df$eig) * 1.1))
    })
  #   # PC plot
  #
pca_biplot <- reactiveValues(pc_plot = NULL)
observeEvent( c(input$plotpca,
                input$the_grouping_variable,
                input$label_points,
                input$draw_ellipse,
                input$the_pcs_to_plot_x,
                input$the_pcs_to_plot_y),{

       if (!(is.null(reactives$matrix) | is.null(reactives$metadata))){

  #
       pcs_df <- pca_objects$pcs_df
       pca_output <-  pca_objects$pca_output
  #
  #     # filter the pca objects for values that should not be plotted
      display_samples <- input$display_samples
      if (!is.null(display_samples)) {
        pcs_df <- pcs_df[which(rownames(pcs_df) %in% display_samples),]
        pca_output$x <- pca_output$x[which(rownames(pca_output$x) %in% display_samples),]
        #pca_output$sdev <- pca_output$sdev[which(rownames(pca_output$sdev) %in% display_samples),]
      }
  #     #
      var_expl_x <-
        round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))] ^
                2 / sum(pca_output$sdev ^ 2), 1)
      var_expl_y <-
        round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))] ^
                2 / sum(pca_output$sdev ^ 2), 1)
      labels <- rownames(pca_output$x)
      grouping <- input$the_grouping_variable
      if (is.null(grouping)) {
        grouping = 'None'
      }
  #     #
  #     #     #TODO: separate the plot + legend since the legends can vary in size considerably
  #     #
      if (grouping == 'None') {
        pc_plot <<- ggplot(pcs_df,
                           aes_string(input$the_pcs_to_plot_x,
                                      input$the_pcs_to_plot_y))
      } else {
        pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
        pc_plot <<- ggplot(
          pcs_df,
          aes_string(
            input$the_pcs_to_plot_x,
            input$the_pcs_to_plot_y,
            colour = 'fill_'
          )
        )
      }

  #
      print(labels)
      if (input$label_points) {
        pc_plot = pc_plot +  geom_text(aes(label = labels),  size = 5)
      } else {
        pc_plot = pc_plot + geom_point()
      }

      pc_plot <- pc_plot +
        theme_gray(base_size = 14)

      if (grouping != 'None') {
        pc_plot <- pc_plot +
          scale_colour_discrete(name = "groups") +
          theme(legend.position = "top")
      }
      #
      if (input$draw_ellipse) {
        pc_plot = pc_plot + stat_ellipse(
          aes(fill = 'fill_'),
          geom = "polygon",
          alpha = 0.1,
          show.legend = FALSE
        )
      }

      pc_plot <- pc_plot +
        coord_equal() +
        xlab(paste0(
          input$the_pcs_to_plot_x,
          " (",
          var_expl_x,
          "% explained variance)"
        )) +
        ylab(paste0(
          input$the_pcs_to_plot_y,
          " (",
          var_expl_y,
          "% explained variance)"
        ))


       pca_biplot$pc_plot <- pc_plot
     }
  #
  #
  })

  #
  #   # This is the main PCA biplot
  #   # TODO: determine how to to make the zoom/reset work in this plot instead of dividing the functionality
  #   #       between two plots


    output$PCA_PLOT <- renderPlot({

      pca_biplot$pc_plot

    })

    #   # This is the zoomed in plot
    #   # for zooming
    output$ZOOMED_PLOT <- renderPlot({
      brush <- input$PCA_PLOTBrush

      if (is.null(brush)) {
        pca_biplot$pc_plot
      } else {
        pca_biplot$pc_plot + coord_cartesian(
          xlim = c(brush$xmin, brush$xmax),
          ylim = c(brush$ymin, brush$ymax)
        )
      }
      #
    })
    #   #
    output$brush_info_after_zoom <- renderTable({
      # get the pca metadata
      the_metadata_subset <- pca_objects$the_metadata
      metadata_cols <- names(the_metadata_subset)

      brush <- input$PCA_PLOTBrush
      the_pca_data <- pca_objects$pcs_df
      if (!is.null(brush)) {
        the_pca_data <- brushedPoints(the_pca_data, brush)
      }

      # now return only the columns from the pca data tha match the metadata colnames
      data.frame(sample=rownames(the_pca_data),the_pca_data[, metadata_cols])

    })
    #   #
    output$pca_details <- renderPrint({
      #
      #print(pca_objects$pca_output$x)
      summary(pca_objects$pca_output)
    })
    #
    #   #Closure not susbatable here, if commenter write something empty error
    #
    #   # Validate the input and set the 'input validated variable'

  #
  #   # # download PCA data
  #   output$downloadPCAOutput <- downloadHandler(
  #     filename = 'PCA_sample_coefficients.tsv',
  #     content = function(file) {
  #       data = pca_objects$pca_output$x
  #       write.table(data, file, sep="\t")
  #     }
  #   )
  #   #
  #   # # download PCA data
  output$downloadPCARotation <- downloadHandler(
    filename = 'PCA_gene_loadings.tsv',
    content = function(file) {
      data = pca_objects$pca_output$rotation
      write.table(data, file, sep="\t")
    }
  )
  #
  #   # # code to return to the input tab when validation fails
  observeEvent(
    input$returnToInput,
    {
      updateTabsetPanel(session, "mainTabPanel", selected="Input")
      removeModal(session)
    }

  )

  # # get the URL of the count file supplied by the user
  output$countFileURL <- renderUI({
    query <- parseQueryString(session$clientData$url_search)

    cfu <- query$countFileURL

    if(!is.null(cfu)) {
      updateRadioButtons(session, ns('count_file_method'), selected = 'download')
    }
    textInput(ns('count_file_url'),'Count file URL:', value=cfu, width=600)

  })
  # #
  output$metadataFileURL <- renderUI({
    query <- parseQueryString(session$clientData$url_search)

    mfu <- query$metadataFileURL
    if(!is.null(mfu)) {
      updateRadioButtons(session, ns('metadata_file_method'), selected = 'download')
    }
    textInput(ns('metadata_file_url'),'Metadata file URL:', value=mfu, width=600)
  })


  return(reactives)

}
