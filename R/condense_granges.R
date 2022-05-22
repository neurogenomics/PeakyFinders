#' Condense \link[GenomicRanges]{GRanges}
#' 
#' Condense a \link[GenomicRanges]{GRanges} object
#' by taking the min/max position per chromosome.
#' This helps to reduce the total number of queries, 
#' which can cause memory allocation problems 
#' due to repeated calls to the underlying C libraries. 
#' @param gr A \link[GenomicRanges]{GRanges} object.  
#' @param split_chromosomes Split into a named list by chromosome. 
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
#' @export
#' @importFrom GenomicRanges GRanges seqnames start end GRangesList
#' @examples
#' gr <- unlist(
#'     GenomicRanges::GRangesList(
#'         "gr1"=GenomicRanges::GRanges("4:1-100"),
#'         "gr2"=GenomicRanges::GRanges("4:300-500"),
#'         "gr3"=GenomicRanges::GRanges("8:40-10000"))
#' )
#' gr2 <- condense_granges(gr = gr)                
condense_granges <- function(gr,
                             split_chromosomes = FALSE){ 
    grlist <- split_chromosomes_run(query_granges = gr)
    grc <- mapply(grlist, FUN=function(x){ 
        gr_str <-  paste0(
            as.character(unique(GenomicRanges::seqnames(x))),
            ":",
            min(GenomicRanges::start(x)),
            "-",
            max(GenomicRanges::end(x))
        )
        # print(gr_str)
        GenomicRanges::GRanges(gr_str)
    }) |> GenomicRanges::GRangesList() 
    if(isFALSE(split_chromosomes)) grc <- unlist(grc)
    return(grc)
}
