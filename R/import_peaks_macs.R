import_peaks_macs <- function(paths,
                              cutoff,
                              query_granges=NULL, 
                              split_files=FALSE,
                              verbose=TRUE){ 
    
    messager("Importing MACS peak files.",v=verbose)
    peaks <- lapply(paths, function(f){ 
        p <- rtracklayer::import(con = f, 
                                 which = query_granges, 
                                 format = "bed")
        p <- add_mcol(gr = p,
                      name = "source", 
                      value =  basename(f)) 
        return(p) 
    }) |> 
        unlist() |> 
        GenomicRanges::GRangesList() |> 
        unlist() 
    ### Add peaktype ###
    peaks <- add_mcol(gr = peaks,
                      name = "peaktype", 
                      value =  paste0("MACS3_cutoff",cutoff)) 
    #### Add metadata #### 
    peaks@metadata <- list(paths=paths,
                           cutoff=cutoff)
    #### Split Granges into GRangesList #####
    if(isTRUE(split_files)){ 
        messager("Splitting GRanges into GRangesList.",v=verbose)
        peaks <- GenomicRanges::split(x = peaks,
                                      f = peaks$source)
    } 
    return(peaks)
}
