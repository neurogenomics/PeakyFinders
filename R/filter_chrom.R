#' Filter chromosomes
#' 
#' Remove non-standard chromosomes from a list of 
#' \link[GenomicRanges]{GRanges} objects.
#' @param grlist Named list of \link[GenomicRanges]{GRanges} objects.
#' @param keep_chr Which chromosomes to keep.
#' @param verbose Print messages.
#' @returns Named list of \link[GenomicRanges]{GRanges} objects.
#' 
#' @importFrom GenomeInfoDb seqlevelsStyle seqnames sortSeqlevels seqlevelsInUse
#' @importFrom BiocGenerics %in% 
#' 
#' @export
#' @examples 
#' gr <- GenomicRanges::GRanges(c("chr6:1-1000","chrUns999:1-1000"))
#' gr2 <- filter_chrom(grlist=gr)
filter_chrom <- function(grlist,
                         keep_chr = paste0(
                             "chr",c(seq_len(22),"X","Y")
                         ),
                         verbose = TRUE){
    
    if(is.null(keep_chr)) return(grlist)
    messager("Removing non-standard chromosomes.",v=verbose)
    #### Check if it's a single element ####
    tmp_list <- FALSE
    if(is_granges(grlist)){
        grlist <- list(gr1=grlist)
        tmp_list <- TRUE
    }
    grlist <- mapply(grlist,
           SIMPLIFY = FALSE,
           FUN = function(gr){ 
               suppressMessages(suppressWarnings(
                   GenomeInfoDb::seqlevelsStyle(gr) <- "UCSC"
               ))
               gr <- gr[BiocGenerics::`%in%`(GenomeInfoDb::seqnames(gr),
                                             keep_chr)]
               GenomeInfoDb::seqlevels(gr) <- GenomeInfoDb::sortSeqlevels(
                   GenomeInfoDb::seqlevelsInUse(gr)
               )
               #update seq lengths
               GenomeInfoDb::seqlevels(
                   gr, pruning.mode="coarse") <- keep_chr
               return(gr)
           })
    if(tmp_list) grlist <- grlist[[1]]
    return(grlist)
}
