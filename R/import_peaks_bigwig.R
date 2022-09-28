import_peaks_bigwig <- function(paths,
                                id,
                                query_granges,
                                build,
                                method,
                                cutoff,
                                peaks_dir,
                                verbose=TRUE){
    
    messager("Computing peaks from bigWig file.",v=verbose)
    is_windows <- rtracklayer_bigwig_error() 
    #### Import bigWig subset #### 
    which <- if(is.null(method)) query_granges else NULL
    peaks_all <- lapply(paths, function(x){
        if((!is.null(query_granges)) &
           (!is.null(method))){
            
            chroms <- as.character(
                unique(GenomicRanges::seqnames(query_granges))
            )
            ## Import the entire chromosome to accurately compute peaks.
            gr <- import_bedgraph_chroms(URL = x, 
                                         chroms = chroms, 
                                         build = build, 
                                         import_format = "BigWig",
                                         verbose = verbose) 
        } else {
            ## Import the entire genome.
            chroms <- "chrALL"
            gr <- rtracklayer::import.bw(con = x, which=which)
        }
        #### Fix seqinfo ####
        gr <- fix_seqinfo(gr = gr, 
                          build = build, 
                          verbose = verbose)
        #### Exit early ####
        if(is.null(method)) {
            messager("Returning bigWig GRanges without computing peaks.",
                     v=verbose)
            GenomicRanges::mcols(gr)$peaktype <- "bigWig"
            return(gr)
        } 
        #### Save lifted subset ####
        messager("Writing bigWig subset as bedGraph.",v=verbose)
        tmp_lifted <- tempfile(fileext = paste(id,".bedgraph",
                                               sep="."))
        rtracklayer::export.bedGraph(object = gr,
                                     con = tmp_lifted)
        #### Call peaks ####
        peaks <- call_peaks(bedgraph_path = tmp_lifted,
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
    GenomicRanges::mcols(peaks_all)$peaktype <- "MACSrPeak_bigwig"
    return(peaks_all)
}
