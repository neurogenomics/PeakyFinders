#' Import peaks: files
#' 
#' Import peaks from local or remote file paths.
#'  
#' @inheritParams import_peaks  
#' @keywords internal
#' @returns \link[GenomicRanges]{GRanges}
import_peaks_files <- function(ids,
                               build = "hg19",
                               query_granges = NULL, 
                               query_granges_build = NULL,
                               split_chromosomes = FALSE,
                               condense_queries = TRUE,
                               force_new = FALSE, 
                               method = "MACSr",
                               cutoff = NULL,
                               searches = construct_searches(),
                               peaks_dir = tempdir(), 
                               nThread = 1,
                               verbose = TRUE){ 
    
    messager("Querying",length(ids),"files.",v=verbose)
    build <- translate_genome(genome = build, 
                              style = "UCSC")
    #### Import data ####
    ids <- stats::setNames(ids,make.unique(basename(ids)))
    grl <- mapply(ids, FUN=function(id){
        messager("Processing file: >>>",id,"<<<",v=verbose)
        import_peaks_multi(links = id,
                           build = build,
                           query_granges = query_granges,
                           query_granges_build = query_granges_build,
                           split_chromosomes = split_chromosomes,
                           condense_queries = condense_queries,
                           method = method,
                           cutoff = cutoff,
                           searches = searches,
                           peaks_dir = peaks_dir,
                           nThread = nThread,
                           verbose = verbose
        )
    })
    return(grl)
}
