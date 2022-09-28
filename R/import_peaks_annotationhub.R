#' Import peaks: AnnotationHub
#' 
#' Import peaks from \href{https://doi.org/doi:10.18129/B9.bioc.AnnotationHub}{
#' AnnotationHub}.
#'  
#' @inheritParams import_peaks  
#' @keywords internal
#' @returns \link[GenomicRanges]{GRanges}
import_peaks_annotationhub <- function(ids,
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
    messager("Querying",length(ids),"id(s) from: AnnotationHub",v=verbose)
    build <- translate_genome(genome = build, 
                              style = "UCSC")
    #### Import metadata ####
    if(all(grepl("peak",names(searches),ignore.case = TRUE))){
        meta <- search_annotationhub(searches = searches,
                                     peaks_only = TRUE,
                                     # genome = build,
                                     verbose = verbose)  
        if(any(startsWith(build,c("hg","GRCh")))){
            build <- "hg19" ## All human builds in this metadata are hg19
        } 
    } else {
        meta <- search_annotationhub(
            searches = searches,
            output_category = paste(unlist(searches),collapse = "|"),
            output_type = paste(unlist(searches),collapse = "|"),
            file_format_type = paste(unlist(searches),collapse = "|"),
            # genome = build, ## Removes all data for some reason
            verbose = verbose
        ) 
        # "hg19" "mm10" "hg38" "dm6"  "dm3" 
    }
    ## Filtering this way works fine tho 
    meta <- filter_genome(meta = meta, 
                          build = build,
                          build_col = "genome")
    if(nrow(meta)==0) {
        messager("Returning NULL.")
        return(NULL)
    }
    #### Get links ####
    links_list <- get_links_annotationhub(meta = meta,
                                          ids = ids,
                                          searches = searches,
                                          build = build,
                                          verbose = verbose)
    #### Import data ####
    ids <- stats::setNames(ids,ids)
    grl <- mapply(ids, FUN=function(id){
        messager("Processing id: >>>",id,"<<<",v=verbose)
        import_peaks_multi(links = links_list[[id]],
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
