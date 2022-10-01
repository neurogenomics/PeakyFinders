import_peaks_broadpeak <- function(paths,
                                   query_granges,
                                   verbose=TRUE){
    messager("Importing pre-computed broadPeak files.",v=verbose)
    peaks <- lapply(paths, function(f){ 
        messager(" -",f,v=verbose)
        p <- rtracklayer::import(con = f, 
                                 which = query_granges,
                                 format = "broadPeak")
        GenomicRanges::mcols(p)$source <- basename(f)
        return(p)
    }) |> 
        unlist() |> 
        GenomicRanges::GRangesList() |> 
        unlist() 
    ### Add to list ###
    GenomicRanges::mcols(peaks)$peaktype <- "broadPeak"
    return(peaks)
}
