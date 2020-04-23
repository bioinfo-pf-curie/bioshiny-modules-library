
gtf_path <- system.file("extdata", "gencode.v19.annotation.sorted.gtf", package = "BioshinyModules")


ensembl2symbol <- function(x, gtf.in, lab.in="gene_id", lab.out="gene_name", annot.only=FALSE, cleanEnsembl=TRUE){
  dgtf <- rtracklayer::import(gtf.in)
  myGenes <- dgtf[dgtf$type == "gene"]

  ## Check if x is a vector or a matrix
  if (is.vector(x)){
    x <- matrix(x, byrow=FALSE, dimnames=list(x,"Ids"))
    annot.only <- TRUE
  }

  ## Check input label (gene_id by default)
  if (is.element(lab.in, colnames(elementMetadata(myGenes)))){
    colsOfInterest <- lab.in
  }else{
    warning("Unable to convert ID to SYMBOL gene names ! Input label not found !")
    return(x)
  }

  ## Try all output labels
  if (is.element(lab.out, colnames(elementMetadata(myGenes)))){
    colsOfInterest <- c(colsOfInterest, lab.out)
  }

  if(length(colsOfInterest) != 2){
    warning("Unable to convert ID to SYMBOL gene names ! Output label not found !")
    return(x)
  }

  mcols(myGenes) <- mcols(myGenes)[colsOfInterest]
  m <- match(rownames(x),  elementMetadata(myGenes)[,lab.in])
  if(length(!is.na(m)) != dim(x)[1]){
    warning("Unable to convert ENSEMBL to SYMBOL gene names ! ")
    return(x)
  }else{
    if (annot.only){
      rnames <- elementMetadata(myGenes)[,lab.in][m]
      if (cleanEnsembl){
        out <- cbind(cleanEnsembl(elementMetadata(myGenes)[,lab.in][m]),
                     elementMetadata(myGenes)[,colsOfInterest[2]][m])
      }else{
        out <- cbind(elementMetadata(myGenes)[,lab.in][m],
                     elementMetadata(myGenes)[,colsOfInterest[2]][m])
      }
      colnames(out) <- c(lab.in, colsOfInterest[2])
      return(out)
    }else{
      if (cleanEnsembl){
        rownames(x) <- paste(cleanEnsembl(elementMetadata(myGenes)[,lab.in][m]),
                             elementMetadata(myGenes)[,colsOfInterest[2]][m], sep="|")
      }else{
        rownames(x) <- paste(elementMetadata(myGenes)[,lab.in][m],
                             elementMetadata(myGenes)[,colsOfInterest[2]][m], sep="|")
      }
      return(x)
    }
  }
}

cleanEnsembl <- function(x){
  if (is.vector(x)){
    x <- gsub("\\.[0-9]*$", "", x)
  }else if (is.matrix(x) || is.data.frame(x)){
    rownames(x) <- gsub("\\.[0-9]*$", "", rownames(x))
  }
  x
}

