#' Import peaks
#' 
#' Import pe-computed peak files, or
#' compute new peaks from bedGraph files.
#' Automatically detects which database each accession ID is from
#' and queries a subset of ranges specified in \code{query_granges}.
#' Currently recognizes IDs from: 
#' \itemize{
#' \item{\href{http://www.ncbi.nlm.nih.gov/geo}{GEO}}
#' \item{\href{https://www.encodeproject.org/}{ENCODE}}
#' \item{\href{http://www.roadmapepigenomics.org/}{ROADMAP}}
#' \item{\href{https://doi.org/doi:10.18129/B9.bioc.AnnotationHub}{
#' AnnotationHub}}
#' } 
#' 
#' @param ids IDs from one of the supported databases. 
#' IDs can be at any level: file, sample, or experiment.
#' @param builds Genome build that each sample in \code{ids} is aligned to.
#' This will determine whether whether the \code{query_granges} data need to be 
#' lifted over to different genome build before querying.
#' Can be a single character string applied to all \code{ids} (e.g. "hg19"), 
#' or a vector of the same length as \code{ids} named using the \code{ids} 
#' (e.g. c("GSM4271282"="hg19", "ENCFF048VDO"="hg38")).  
#' @param query_granges [Optional]
#' \link[GenomicRanges]{GRanges} object indicating which genomic
#' regions to extract from each sample.
#' @param query_granges_build [Optional]
#' Genome build that \code{query_granges} is aligned to.
#' @param force_new By default, saved results of the same \code{save_path} name 
#' will be imported instead of running queries. However you can override this
#' by setting \code{force_new} to perform new queries regardless and overwrite
#'  the old \code{save_path} file.
#' @param peaks_dir Directory to save peaks to 
#' (only used when calling peaks from bedGraph files).
#' @param save_path Path to save query results to in \emph{.rds} format.
#' @param split_chromosomes Split single-threaded query 
#' into multi-threaded query across chromosomes. 
#' This is can be helpful especially when calling peaks from 
#' large bigWig/bedGraph files.
#' The number of threads used is set by the \code{nThread} argument. 
#' @param condense_queries Condense \code{query_granges}
#' by taking the min/max position per chromosome (default: \code{TRUE)}.
#' This helps to reduce the total number of queries, 
#' which can cause memory allocation problems 
#' due to repeated calls to the underlying C libraries. 
#' @param nThread Number of threads to parallelize across.
#' @param verbose Print messages.
#' @inheritParams call_peaks
#' @inheritParams construct_searches 
#' 
#' @returns 
#' A named list of peak files in  \link[GenomicRanges]{GRanges} format.
#' If peaks could not be recovered for a sample,
#'  that element will be filled with \code{NA}.
#' 
#' @export
#' @importFrom stats setNames 
#' @importFrom GenomicRanges GRanges
#' @examples
#' grl <- PeakyFinders::import_peaks(
#'     ids = c("GSM945244"),# "ENCSR000AHD"
#'     searches = PeakyFinders::construct_searches(keys = "narrowpeak"))
import_peaks <- function(ids,
                         builds = "hg19",
                         query_granges = NULL, 
                         query_granges_build = NULL,
                         split_chromosomes = FALSE,
                         condense_queries = TRUE,
                         force_new = FALSE, 
                         call_peaks_method = "MACSr",
                         cutoff = NULL,
                         searches = construct_searches(),
                         peaks_dir = tempdir(),
                         save_path = tempfile(
                             fileext = "_PeakyFinders_grl.rds"
                         ),
                         nThread = 1,
                         verbose = TRUE){
    
    #### Check builds ####
    if(length(builds)>1 && (length(builds)!=length(ids))){
        stop("builds must be same length as ids.")
    } 
    #### Get GSM names ####
    id_list <- process_ids(ids = ids, 
                           verbose = verbose) 
    #### Check query_granges ####
    if(!is.null(query_granges)){
        ## Standardise to UCSC style 
        query_granges <- dt_to_granges(query_granges, 
                                       style = "UCSC", 
                                       verbose = verbose)
        if(is.null(query_granges_build)){
            stopper("query_granges_build must be",
                    "set when using query_granges.")
        }
    } 
    #### Check for pre-existing data ####
    if((!is.null(save_path)) && 
       file.exists(save_path) & 
       isFALSE(force_new)){
        messager("Importing stored peaks data.",v=verbose)
        out_list <- readRDS(save_path)
    } else { 
        out_list <- list()
        # build <- parse_build(builds=builds,
        #                      id=id,
        #                      verbose=TRUE)
        #### Import GEO data ####
        if(length(id_list$GEO)>0){
            out_list[["GEO"]] <- import_peaks_geo(
                ids = id_list$GEO,  
                build = builds,
                query_granges = query_granges,
                query_granges_build = query_granges_build,
                split_chromosomes = split_chromosomes,
                call_peaks_method = call_peaks_method,
                cutoff = cutoff,
                searches = searches,
                peaks_dir = peaks_dir, 
                nThread = nThread,
                verbose = verbose)
        }
        #### Import ENCODE data ####
        if(length(id_list$ENCODE)>0){
            out_list[["ENCODE"]] <- import_peaks_encode(
                ids = id_list$ENCODE,  
                build = builds,
                query_granges = query_granges,
                query_granges_build = query_granges_build,
                split_chromosomes = split_chromosomes,
                call_peaks_method = call_peaks_method,
                cutoff = cutoff,
                searches = searches,
                peaks_dir = peaks_dir, 
                nThread = nThread,
                verbose = verbose)
        } 
        #### Import ROADMAP data ####
        if(length(id_list$ROADMAP)>0){
            out_list[["ROADMAP"]] <- import_peaks_roadmap(
                ids = id_list$ROADMAP,  
                build = builds,
                query_granges = query_granges,
                query_granges_build = query_granges_build,
                split_chromosomes = split_chromosomes,
                call_peaks_method = call_peaks_method,
                cutoff = cutoff,
                searches = searches,
                peaks_dir = peaks_dir, 
                nThread = nThread,
                verbose = verbose)
        } 
        #### Import ROADMAP data ####
        if(length(id_list$AnnotationHub)>0){
            out_list[["AnnotationHub"]] <- import_peaks_annotationhub(
                ids = id_list$AnnotationHub,  
                build = builds,
                query_granges = query_granges,
                query_granges_build = query_granges_build,
                split_chromosomes = split_chromosomes,
                call_peaks_method = call_peaks_method,
                cutoff = cutoff,
                searches = searches,
                peaks_dir = peaks_dir, 
                nThread = nThread,
                verbose = verbose)
        } 
        #### Save ####
        if(!is.null(save_path)){
            messager("Saving results ==> ",save_path,v=verbose)
            dir.create(dirname(save_path),
                       showWarnings = FALSE, recursive = TRUE)
            saveRDS(out_list, save_path)
        } 
    }
    return(out_list)    
}
