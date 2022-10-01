#' Import peaks: ENCODE
#' 
#' Import peaks from the Encyclopedia of DNA Elements
#'  (\href{https://www.encodeproject.org/}{ENCODE}).
#'  
#' @inheritParams import_peaks 
#' @source {https://github.com/neurogenomics/EpiCompare/issues/43}{
#' Discussion on EpiCompare GitHub}
#' @keywords internal
#' @returns Named list of \link[GenomicRanges]{GRanges}.
import_peaks_encode <- function(ids,
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
    # DeepBlueR::deepblue_name_to_id(name = "ENCFF793BFF", collection = "Samples")
    # query_id <- DeepBlueR::deepblue_list_experiments(
    #     sample =c("ENCSR080UMK")
    #     # ) 
    messager("Querying",length(ids),"id(s) from: ENCODE",v=verbose)  
    build <- translate_genome(genome = build, 
                              style = "Ensembl")
    #### Import metadata ####
    if(all(grepl("peak",names(searches),ignore.case = TRUE))){
        meta <- search_encode(peaks_only = TRUE)  
    } else {
        meta <- search_encode(
            file_type = paste(unlist(searches),collapse = "|"), 
            verbose = verbose
        )
    }
    meta <- filter_genome(meta = meta, 
                          build = build,
                          build_col = "assembly")
    if(nrow(meta)==0) {
        messager("Returning NULL.")
        return(NULL)
    }
    #### Get links ####
    links_list <- get_links_encode(meta = meta,
                                   ids = ids,
                                   searches = searches,
                                   verbose = verbose)
    #### Import data ####
    ids <- stats::setNames(ids,ids)
    grl <- mapply(ids, 
                  SIMPLIFY = FALSE,
                  FUN=function(id){
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
