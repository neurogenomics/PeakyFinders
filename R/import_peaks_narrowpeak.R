import_peaks_narrowpeak <- function(paths,
                                    query_granges,
                                    verbose=TRUE){
    
    messager("Importing",formatC(length(paths),big.mark = ","),
             "pre-computed narrowPeak files.",v=verbose)
    peaks <- lapply(paths, 
                    function(f){ 
        messager(" -",f,v=verbose)               
        #### Handle narrowPeak files in bigBed format ####
        if(grepl("bigbed",f,ignore.case = TRUE)){
            #### Bug occurs when which=NULL #### 
            p <- tryCatch({
                if(is.null(query_granges)){
                    rtracklayer::import(con = f)
                } else {
                    rtracklayer::import(con = f, 
                                        which = query_granges)
                }  
            }, error=function(e){message(e);NULL})
            if(is.null(p)){
                p <- import_peaks_genericpeak(paths = f, 
                                              query_granges = query_granges, 
                                              verbose = verbose)
            }
            GenomicRanges::mcols(p)$peaktype <- "narrowPeak_bigBed"
        } else {
            p <- rtracklayer::import(con = f, 
                                     which = query_granges, 
                                     format = "narrowPeak")
            GenomicRanges::mcols(p)$peaktype <- "narrowPeak"
        } 
        GenomicRanges::mcols(p)$source <- basename(f)
        return(p)
    }) |> 
        unlist() |> 
        GenomicRanges::GRangesList() |> 
        unlist()  
    return(peaks)
}
