#' Import peaks: ROADMAP
#' 
#' Import peaks from the \href{http://www.roadmapepigenomics.org}{
#' NIH Roadmap Epigenomics Mapping Consortium}.
#'  
#' @inheritParams import_peaks  
#' @keywords internal
#' @returns \link[GenomicRanges]{GRanges}.
import_peaks_roadmap <- function(ids,
                                build = "hg19",
                                query_granges = NULL, 
                                query_granges_build = NULL,
                                split_chromosomes = FALSE,
                                condense_queries = TRUE,
                                force_new = FALSE, 
                                call_peaks_method = "MACSr",
                                cutoff = NULL,
                                searches = construct_searches(),
                                peaks_dir = tempdir(), 
                                nThread = 1,
                                verbose = TRUE){  
    genome <- NULL;
    
    messager("Querying",length(ids),"id(s) from: ROADMAP",v=verbose)  
    ### All data in AnnotationHub ROADMAP is in hg19 ####
    build <- "hg19" 
    #### Import metadata ####
    if(all(grepl("peak",names(searches),ignore.case = TRUE))){
        meta <- search_roadmap(peaks_only = TRUE)  
    } else {
        meta <- search_roadmap(
            output_category = paste(unlist(searches),collapse = "|"),
            output_type = paste(unlist(searches),collapse = "|"),
            file_format_type = paste(unlist(searches),collapse = "|"),
            # genome = build, ## Removes all data for some reason
            verbose = verbose
        ) 
    } 
    meta <- filter_genome(meta = meta, 
                          build = build,
                          build_col = "genome")
    #### Exit early ####
    if(nrow(meta)==0) {
        messager("Returning NULL.")
        return(NULL)
    }
    #### Get links ####
    links_list <- get_links_roadmap(meta = meta,
                                    ids = ids,
                                    searches = searches,
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
                           call_peaks_method = call_peaks_method,
                           cutoff = cutoff,
                           searches = searches,
                           peaks_dir = peaks_dir,
                           nThread = nThread,
                           verbose = verbose
        )
    })
    return(grl)
}
