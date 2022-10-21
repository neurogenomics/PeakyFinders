dropna_granges <- function(gr,
                           cols="score"){
    
    cols <- cols[cols %in% names(GenomicRanges::mcols(gr))]
    for(cl in cols){ 
        gr <- gr[!is.na(GenomicRanges::mcols(gr)[["score"]]),]
    }
    return(gr)
}