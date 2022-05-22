#' Clean GenomicRanges
#' 
#' Remove known problematic columns from a 
#' \link[GenomicRanges]{GRanges} object. 
#' @param gr \link[GenomicRanges]{GRanges} or \link[data.table]{data.table}. 
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
#' @keywords internal
#' @importFrom GenomicRanges elementMetadata
#' @importFrom methods is
clean_granges <- function(gr) {
    no_no_cols <- c(
        "seqnames", "ranges", "strand", "seqlevels", "seqlengths",
        "isCircular", "start", "end", "width", "element"
    )
    if(methods::is(gr,"data.frame")){
        keep_cols <- colnames(gr)[
            (!duplicated(toupper(colnames(gr)))) &
                (toupper(colnames(gr)) %in% toupper(no_no_cols)) 
        ] 
        gr <- gr[,keep_cols,drop=FALSE]
    } else if (methods::is(gr,"GRanges")){
        metadat <- GenomicRanges::elementMetadata(gr)
        GenomicRanges::elementMetadata(gr) <-
            metadat[, !colnames(metadat) %in% no_no_cols]
    } else {
        stop("gr must be of class GRanges or data.frame.")
    } 
    return(gr)
}
