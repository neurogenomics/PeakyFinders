add_mcol <- function(gr,
                     name,
                     value,
                     verbose=FALSE){
    if(length(gr)==0){
        messager("WARNING: GRanges contains 0 rows.",v=verbose)
        return(gr)
    } else {
        GenomicRanges::mcols(gr)[[name]] <- value
        return(gr)
    } 
    return(gr)
}