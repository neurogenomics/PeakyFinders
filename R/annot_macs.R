annot_macs <- function(gr, 
                       path,
                       verbose=TRUE){

    vcols <- grep("^V[0-9]+$",names(GenomicRanges::mcols(gr)))
    if(length(vcols)==0) return(gr)
    gr <- gr[,!unlist(lapply(GenomicRanges::mcols(gr), 
                             function(x){
                                 methods::is(x,"character") &&
                                     all(x==".")}))]
    
    if(isTRUE(is_macs(gr=gr, path=path))){ 
       if(length(vcols) %in% c(5,6)){
           messager("Annotating inferred MACS columns.",v=verbose)
           names(GenomicRanges::mcols(gr))[seq_len(length(vcols))] <- 
               c("name","score","signalValue",
                 "pValue","qValue","peak")[seq_len(length(vcols))]
       }
    }
    return(gr)
}
