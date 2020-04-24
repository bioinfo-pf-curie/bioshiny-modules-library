#
# gtf_path <- system.file("extdata", "gencode.v19.annotation.sorted.gtf", package = "PdxAppPackage")
# gtf.in <- gtf_path
#
# counts_path <- system.file("extdata", "rawcounts.csv", package = "BioshinyModules")
#
#
# counts <- read.table(counts_path, header = TRUE, sep = ",",
#                                             row.names = 1)
#
# library(GenomicFeatures)
# library(rtracklayer)
#
# ensembl2symbol <- function(x, gtf.in, lab.in="gene_id", lab.out="gene_name", annot.only=FALSE, cleanEnsembl=TRUE){
#   dgtf <- rtracklayer::import(gtf.in)
#   myGenes <- dgtf[dgtf$type == "gene"]
#
#   ## Check if x is a vector or a matrix
#   if (is.vector(x)){
#     x <- matrix(x, byrow=FALSE, dimnames=list(x,"Ids"))
#     annot.only <- TRUE
#   }
#
#   ## Check input label (gene_id by default)
#   if (is.element(lab.in, colnames(elementMetadata(myGenes)))){
#     colsOfInterest <- lab.in
#   }else{
#     warning("Unable to convert ID to SYMBOL gene names ! Input label not found !")
#     return(x)
#   }
#
#   ## Try all output labels
#   if (is.element(lab.out, colnames(elementMetadata(myGenes)))){
#     colsOfInterest <- c(colsOfInterest, lab.out)
#   }
#
#   if(length(colsOfInterest) != 2){
#     warning("Unable to convert ID to SYMBOL gene names ! Output label not found !")
#     return(x)
#   }
#
#   mcols(myGenes) <- mcols(myGenes)[colsOfInterest]
#   m <- match(rownames(x),  elementMetadata(myGenes)[,lab.in])
#   if(length(!is.na(m)) != dim(x)[1]){
#     warning("Unable to convert ENSEMBL to SYMBOL gene names ! ")
#     return(x)
#   }else{
#     if (annot.only){
#       rnames <- elementMetadata(myGenes)[,lab.in][m]
#       if (cleanEnsembl){
#         out <- cbind(cleanEnsembl(elementMetadata(myGenes)[,lab.in][m]),
#                      elementMetadata(myGenes)[,colsOfInterest[2]][m])
#       }else{
#         out <- cbind(elementMetadata(myGenes)[,lab.in][m],
#                      elementMetadata(myGenes)[,colsOfInterest[2]][m])
#       }
#       colnames(out) <- c(lab.in, colsOfInterest[2])
#       return(out)
#     }else{
#       if (cleanEnsembl){
#         rownames(x) <- paste(cleanEnsembl(elementMetadata(myGenes)[,lab.in][m]),
#                              elementMetadata(myGenes)[,colsOfInterest[2]][m], sep="|")
#       }else{
#         rownames(x) <- paste(elementMetadata(myGenes)[,lab.in][m],
#                              elementMetadata(myGenes)[,colsOfInterest[2]][m], sep="|")
#       }
#       return(x)
#     }
#   }
# }
#
# cleanEnsembl <- function(x){
#   if (is.vector(x)){
#     x <- gsub("\\.[0-9]*$", "", x)
#   }else if (is.matrix(x) || is.data.frame(x)){
#     rownames(x) <- gsub("\\.[0-9]*$", "", rownames(x))
#   }
#   x
# }
#
# ## getTPM
# ## Calculate TPM values from a gene expression matrix and a gtf file
# ## x : matrix of counts
# ## exonic.gene.size : vector of exonic sizes per gene. If not provided, gtf.in is used to build it
# ## gtf.in : path to gtf file
# ## Details : see http://www.rna-seqblog.com/rpkm-fpkm-and-tpm-clearly-explained/ for differences between RPKM and TPM
#
# getTPM <- function(x, exonic.gene.sizes=NULL, gtf.in=NULL){
#
#   stopifnot(require(GenomicFeatures))
#
#   ## First, import the GTF-file that you have also used as input for htseq-count
#   if (is.null(exonic.gene.sizes)){
#     if (is.null(gtf.in))
#       stop("Unable to calculate gene size")
#     exonic.gene.sizes <- getExonicGeneSize(gtf.in)
#   }
#
#   if (length(setdiff(rownames(x), names(exonic.gene.sizes)))>0){
#     warning(length(setdiff(rownames(x), names(exonic.gene.sizes))), " genes from table were not found in the gtf file ...")
#   }
#
#   exonic.gene.sizes <- exonic.gene.sizes[rownames(x)]
#
#   ## Calculate read per kilobase
#   rpk <- x * 10^3/matrix(exonic.gene.sizes, nrow=nrow(x), ncol=ncol(x), byrow=FALSE)
#   ## Then normalize by lib size
#   tpm <- rpk *  matrix(10^6 / colSums(rpk, na.rm=TRUE), nrow=nrow(rpk), ncol=ncol(rpk), byrow=TRUE)
#
#   return(round(tpm,2))
# }##getTPM
#
#
# ## getExonicGeneSize
# ## Calcule the exons size per gene for RPMKM/TPM normalization
# ## gtf.in : path to gtf file
# getExonicGeneSize <- function(gtf.in){
#   stopifnot(require(GenomicFeatures))
#   txdb <- makeTxDbFromGFF(gtf.in,format="gtf")
#   ## then collect the exons per gene id
#   exons.list.per.gene <- exonsBy(txdb,by="gene")
#   ## then for each gene, reduce all the exons to a set of non overlapping exons, calculate their lengths (widths) and sum then
#   exonic.gene.sizes <- sum(width(reduce(exons.list.per.gene)))
#   return(exonic.gene.sizes)
# }## getExonicGeneSize
#
#
