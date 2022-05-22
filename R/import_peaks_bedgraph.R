import_peaks_bedgraph <- function(paths, 
                                  id,
                                  query_granges, 
                                  build,
                                  call_peaks_method,
                                  cutoff,
                                  peaks_dir,
                                  verbose=TRUE){
    #### Import bedGraph subset #### 
    which <- if(is.null(call_peaks_method)) query_granges else NULL
    peaks_all <- lapply(paths, function(x){
        if((!is.null(query_granges)) &
           (!is.null(call_peaks_method))){
            ## Import the entire chromosome to accurately compute peaks.
            chroms <- as.character(
                unique(GenomicRanges::seqnames(query_granges))
            )
            gr <- import_bedgraph_chroms(URL = x, 
                                         chroms = chroms, 
                                         build = build, 
                                         import_format = "bedGraph", 
                                         verbose = verbose) 
        } else {
            ## Import the entire genome.
            chroms <- "chrALL"
            gr <- rtracklayer::import.bedGraph(con = x, which = which)
        } 
        #### Exit early ####
        if(is.null(call_peaks_method)) {
            messager("Returning bedGraph GRanges without computing peaks.",
                     v=verbose)
            GenomicRanges::mcols(gr)$peaktype <- "bedGraph"
            return(gr)
        } 
        #### Save lifted subset ####
        messager("Writing bedGraph subset.",v=verbose)
        tmp_lifted <- tempfile(
            fileext = paste(id,"bedgraph",sep=".")
        )
        rtracklayer::export.bedGraph(object = gr,
                                     con = tmp_lifted)
        #### Call peaks ####
        peaks <- call_peaks(bedgraph_path = tmp_lifted,
                            call_peaks_method = call_peaks_method,
                            cutoff = cutoff,
                            outdir = peaks_dir,
                            outputfile = paste(
                                id,
                                paste(chroms,collapse = ";"),
                                "peaks.bed",
                                sep=".")
        ) 
        return(peaks)
    }) |> 
        GenomicRanges::GRangesList() |>
        unlist()
    ### Add to list ###
    GenomicRanges::mcols(peaks_all)$peaktype <- "MACSrPeak_bedgraph"
    return(peaks_all)
}
