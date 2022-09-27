import_peaks_seacr <- function(paths,
                               control,
                               query_granges=NULL,
                               split_files=FALSE,
                               nThread=1,
                               verbose=TRUE){
    
    messager("Importing SEACR peak files.",v=verbose)
    peaks <- import_peaks_genericpeak(paths = paths, 
                                      verbose = FALSE) 
    #### Exit early ####
    if(length(peaks)==0){
        messager("WARNING: 0 peaks were called.",v=verbose)
        return(peaks)
    } else {
        messager(formatC(length(peaks),big.mark = ","),
                 "peaks were called.",v=verbose)
    }
    names(GenomicRanges::mcols(peaks))[seq_len(3)] <- c("total_signal",
                                                        "max_signal",
                                                        "max_signal_region")
    #### Add peak type ####
    GenomicRanges::mcols(peaks)["peaktype"] <- 
        paste(
            ifelse(grepl("stringent",peaks$source),"SEACR_stringent",
                   ifelse(grepl("relaxed",peaks$source),"SEACR_stringent",
                          "SEACR")),
            paste0("control",control),
            sep = "_"
        )
    peaks@metadata <- list(paths=paths,
                           control=control)
    #### Split Granges into GRangesList #####
    if(isTRUE(split_files)){ 
        messager("Splitting GRanges into GRangesList.",v=verbose)
        peaks <- GenomicRanges::split(x = peaks,
                                      f = peaks$source)
    } 
    return(peaks)
}
