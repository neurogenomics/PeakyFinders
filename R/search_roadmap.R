#' Search metadata: ENCODE
#' 
#' Search metadata from the Encyclopedia of DNA Elements
#'  (\href{https://www.encodeproject.org/}{ENCODE}).  
#' @param description Search "description" column. 
#' @param rdataclass Search "rdataclass" column. 
#' @param sourcetype Search "sourcetype" column.    
#' @param file_format_type Search "file_format_type" column. 
#' @param output_category Search "output_category" column. 
#' @param output_type Search "output_type" column. 
#' @param biosample_id Search "biosample_id" column. 
#' @param biosample_type Search "biosample_type" column. 
#' @param biosample_name Search "biosample_name" column. 
#' @param dataset_biosample_summary Search "dataset_biosample_summary" column. 
#' @param dataprovider Search "dataprovider" column. 
#' @param species Search "species" column. 
#' @param genome Search "genome" column. 
#' 
#' @param partial_match Return case-insensitive substring matches instead of 
#' exact matches only (default: \code{TRUE}). 
#' @param peaks_only Return pre-filtered metadata for peak data 
#' aligned to the GRCh38 human genome build. 
#' @param verbose Print messages. 
#' @param ... Additional metdata columns to filter.
#' 
#' @source {https://github.com/neurogenomics/EpiCompare/issues/43}{
#' Discussion on EpiCompare GitHub.}
#' @source {https://biodatascience.github.io/compbio/bioc/anno.html}{
#' Mike Love tutorial on accessing peak data from AnnotationHub.}
#' 
#' @returns \link[data.table]{data.table}. 
#' 
#' @export
#' @importFrom AnnotationHub AnnotationHub query 
#' @examples
#' meta <- search_roadmap(biosample_type="brain",
#'                        file_format_type="narrowPeak",
#'                        genome="hg19",
#'                        peaks_only=TRUE)
search_roadmap <- function(## Filter args
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
    #### Import metadata #### 
    if(isFALSE(peaks_only)){
        messager("Importing metadata via AnnotationHub.",v=verbose)
        ah <- AnnotationHub::AnnotationHub()
        dm <- AnnotationHub::query(ah, c("EpigenomeRoadMap"))
        meta <- data.table::data.table(data.frame(AnnotationHub::mcols(dm)), 
                                       keep.rownames = "ah_id")
        #### Spread the nested "tags" col into multiple cols ####
        meta <- unnest_wider_dt(dt = meta, 
                                col = "tags",
                                id_col = "title",
                                new_cols = c("database",
                                             "output_category",
                                             "output_type",
                                             ## Mixture of file type and assay.
                                             "file_format_type", 
                                             "biosample_id",
                                             "biosample_type",
                                             "biosample_name",
                                             "dataset_biosample_summary"
                                ),
                                verbose = verbose)
    } else {
        messager("Subsetting metadata to only peaks.",v=verbose)
        meta <- peaks_metadata_roadmap
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
