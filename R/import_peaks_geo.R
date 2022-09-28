#' Import peaks: GEO
#' 
#' Import narrow/broad/generic peaks from GEO, or compute peaks with 
#' \link[PeakyFinders]{call_peaks}.
#' 
#' Must import \link[methods]{new} in my function
#' because it seems \link[GEOquery]{getGEO} forgot to do this 
#' (only works when you load the entire \pkg{GEOquery} package first).
#' @source \href{https://github.com/ropensci/rtweet/issues/229}{
#' curl connection timeout issues}
#' @source \href{https://github.com/Bioconductor/BiocParallel/issues/111}{
#' Issue with importing bigWig files in parallel}
#' @param gsm GEO GSM id (e.g. "GSM4271282").
#' @inheritParams import_peaks
#' @inheritParams get_geo_links
#' @inheritParams base::options 
#' 
#' @returns Named list of peak files in \link[GenomicRanges]{GRanges} format.
#' 
#' @keywords internal 
#' @importFrom BiocGenerics %in%
#' @importFrom GenomicRanges seqnames mcols GRangesList GRanges mcols
#' @importFrom rtracklayer import import.bedGraph export.bedGraph
#' @importFrom data.table fread 
#' @importFrom httr set_config config
import_peaks_geo <- function(ids,  
                             build,
                             query_granges,
                             query_granges_build,
                             split_chromosomes = FALSE,
                             method = "MACSr",
                             cutoff = NULL,
                             searches = construct_searches(),
                             condense_queries = TRUE,
                             peaks_dir = tempdir(),
                             timeout = 3*60,
                             nThread = 1,
                             verbose = TRUE){ 
    
    messager("Querying",length(ids),"id(s) from: GEO",v=verbose)  
    #### Set timeout ####
    options(timeout = timeout)
    # opts <- httr::timeout(seconds = timeout) 
    httr::set_config(config = httr::config(connecttimeout = timeout),
                     override = TRUE)
    #### Import peaks ####
    ids <- stats::setNames(ids,ids)
    grl <- mapply(ids, FUN=function(id){
        messager("Processing id: >>>",id,"<<<",v=verbose) 
        #### Get links to supplementary files on GEO ####
        links <- get_geo_links(gsm = id,
                               searches = searches,
                               verbose = verbose)
        import_peaks_multi(links = links,
                           id = id,
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
