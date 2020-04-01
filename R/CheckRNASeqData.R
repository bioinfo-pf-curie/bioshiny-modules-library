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


CheckRNAseqDataUI <- function(id) {
  ns <- NS(id)


  actionButton(ns("checkdata"),"Check Data")


}



#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML


CheckRNAseqDataServer <- function(input, output, session, matrix = NULL, annotation = NULL, metadata = NULL) {

  validationModal <- function(msg = "", title = "Validation failed") {
    showModal(modalDialog(p(msg),
                          title = title,
                          footer = tagList(
                            modalButton("Dismiss"),
                            actionButton("returnToInput", "Return To Input Tab")
                          )))

}


reactives <- reactiveValues(matrix = NULL, metadata= NULL)

reactives$matrix <- matrix
reactives$metadata <- metadata



observeEvent(reactives$matrix, {

  if(!is.null(matrix)){


  the_data <- reactives$matrix


  num_rows <- length(the_data[, 1])
  num_cols <- length(the_data)

  # check if it is small
  if (num_cols < 3) {
    validationModal(
      msg = paste(
        "Count dataset seems small with ",
        num_rows,
        " rows and ",
        num_cols,
        " columns. Check that it is formatted correctly and the proper delimiter was selected and try again.  See the README for more information on the format."
      )
    )
    return(-1)
  }
}
})


observeEvent(reactives$metadata,{
  # # now load the metadata

  if(!is.null(metadata)){

  the_metadata <- reactives$metadata

  num_rows <- length(the_metadata[, 1])
  num_cols <- length(the_metadata)

  # check if it is small
  if (num_cols < 1) {
    validationModal(
      msg = paste(
        "Metadata dataset seems small with ",
        num_rows,
        " rows and ",
        num_cols,
        " columns. Check that it is formatted correctly and the proper delimiter was selected and try again.  See the README for more information on the format."
      )
    )
    return(-1)
  }
}
})


observeEvent(input$checkdata,{


  if (!(is.null(matrix) | is.null(metadata))){
    the_metadata <- reactives$metadata
    the_data <- reactives$matrix
    # check that all samples from the count data are present in the metadata and vice versa
    metadata_names <- rownames(the_metadata)
    countdata_names <- rownames(the_data)

    countdata_missing_from_metadata <-
      countdata_names[!(countdata_names %in% metadata_names)]
    metadata_missing_from_countdata <-
      metadata_names[!(metadata_names %in% countdata_names)]

    if (length(countdata_missing_from_metadata) > 0) {
      missing_names_string = paste(countdata_missing_from_metadata, sep = ",")
      validationModal(
        msg = paste(
          "Some sample names from the count data are missing from the metadata:\n",
          missing_names_string,
          sep = ""
        )
      )
      return(-1)
    }

    if (length(metadata_missing_from_countdata) > 0) {
      missing_names_string = paste(metadata_missing_from_countdata, sep = ",")
      validationModal(
        msg = paste(
          "Some sample names from the metadata are missing from the countdata:\n",
          missing_names_string,
          sep = ""
        )
      )
      return(-1)
    }

    #TODO:  if we get here, set the data_validated variable to 1
    data_validated <- 1

    validationModal(msg = "Input looks good!", title = "Validation passed!")
  }
})



}
