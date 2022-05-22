#' Search metadata: AnnotationHub
#' 
#' Search metadata from 
#' \href{https://doi.org/doi:10.18129/B9.bioc.AnnotationHub}{AnnotationHub}.
#' @param ah_id Search "ah_id" column.  
#' @param output_category Search "output_category" column. 
#' @param output_type Search "output_type" column. 
#' @param file_format_type Search "file_format_type" column. 
#' @param dataset_biosample_summary Search "dataset_biosample_summary" column. 
#' @param dataprovider Search "dataprovider" column. 
#' @param species Search "species" column. 
#' @param genome Search "genome" column. 
#' @param description Search "description" column. 
#' @param rdataclass Search "rdataclass" column. 
#' @param sourcetype Search "sourcetype" column. 
#' @param biosample_id Search "biosample_id" column. 
#' @param biosample_type Search "biosample_type" column. 
#' @param biosample_name Search "biosample_name" column. 
#' 
#' @param partial_match Return case-insensitive substring matches instead of 
#' exact matches only (default: \code{TRUE}). 
#' @param peaks_only Return pre-filtered metadata for peak data 
#' aligned to the GRCh38 human genome build. 
#' @param verbose Print messages. 
#' @param ... Additional metadata columns to filter. 
#' @inheritParams import_peaks
#' 
#' @returns \link[data.table]{data.table}. 
#' 
#' @source {https://biodatascience.github.io/compbio/bioc/anno.html}{
#' Mike Love tutorial on accessing peak data from AnnotationHub.}
#' @export
#' @importFrom AnnotationHub AnnotationHub query 
#' @examples
#' meta <- search_annotationhub(searches=construct_searches(keys="narrowpeak"),
#'                              biosample_type="brain",
#'                              peaks_only=TRUE)
search_annotationhub <- function(searches=construct_searches(),
                                ## Filter args
                                ah_id=NULL,
                                output_category=NULL,
                                output_type=NULL,
                                file_format_type=NULL,
                                biosample_id=NULL,
                                biosample_type=NULL,
                                biosample_name=NULL,
                                dataset_biosample_summary=NULL, 
                                
                                dataprovider=NULL,
                                species=NULL,
                                genome=NULL,
                                description=NULL,
                                rdataclass=NULL,
                                sourcetype=NULL,
                                
                                
                                ## Function args
                                partial_match=TRUE,
                                peaks_only=FALSE,
                                verbose=TRUE,
                                ...
                            ){
    
    search_strings <- paste(unname(unlist(searches)),collapse = "|")
    #### Import metadata #### 
    if(isFALSE(peaks_only)){
        messager("Importing metadata via AnnotationHub.",v=verbose)
        ah <- AnnotationHub::AnnotationHub()
        dm <- AnnotationHub::query(ah, search_strings)
        messager(formatC(length(dm),big.mark = ","),
                 "matching records retrieved.",v=verbose)
        meta <- data.table::data.table(data.frame(AnnotationHub::mcols(dm)), 
                                       keep.rownames = "ah_id")
        #### Spread the nested "tags" col into multiple cols ####
        meta <- unnest_wider_dt(dt = meta, 
                                col = "tags",
                                id_col = "title",
                                new_cols = NULL,
                                verbose = verbose)
    } else {
        messager("Subsetting metadata to only peaks.",v=verbose)
        meta <- peaks_metadata_annotationhub
    }
    #### Get arguments ####
    arg_list <- as.list(match.call(definition = search_roadmap))[-1]
    meta <- filter_by_args(meta = meta,
                           arg_list = arg_list,
                           partial_match = partial_match,
                           verbose = verbose)
    #### Report ####
    messager("Returning filtered metadata:",
             formatC(nrow(meta),big.mark = ","),
             "entries.",v=verbose)
    return(meta)
}
