import_peaks_bigbed <- function(paths,
                                query_granges,
                                verbose=TRUE){
    
    messager("Importing",formatC(length(paths),big.mark = ","),
             "pre-computed bigBed files.",v=verbose)
    peaks <- lapply(paths, 
                    function(f){ 
                        messager(" -",f,v=verbose)                
                        #### Bug occurs when which=NULL #### 
                        if(is.null(query_granges)){
                            p <- rtracklayer::import(con = f)
                        } else {
                            p <- rtracklayer::import(con = f, 
                                                     which=query_granges)
                        } 
                        GenomicRanges::mcols(p)$source <- basename(f)
                        return(p)
                    }) |> 
        unlist() |> 
        GenomicRanges::GRangesList() |> 
        unlist() 
    ### Add to list ###
    GenomicRanges::mcols(peaks)$peaktype <- "bigBed"
    return(peaks)
}
