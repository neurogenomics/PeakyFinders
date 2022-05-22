#' Search metadata: ENCODE
#' 
#' Search metadata from the Encyclopedia of DNA Elements
#'  (\href{https://www.encodeproject.org/}{ENCODE}).
#' @param assay Search "assay" column. 
#' @param accession Search "accession" column. 
#' @param file_accession Search "file_accession" column. 
#' @param file_type Search "file_type" column. 
#' @param file_format Search "file_format" column. 
#' @param output_category Search "output_category" column. 
#' @param biosample_id Search "biosample_id" column. 
#' @param biosample_type Search "biosample_type" column. 
#' @param biosample_name Search "biosample_name" column. 
#' @param organism Search "organism" column. 
#' @param dataset_type Search "dataset_type" column. 
#' @param assembly Search "assembly" column. 
#' @param genome_annotation Search "genome_annotation" column. 
#' 
#' @param partial_match Return case-insensitive substring matches instead of 
#' exact matches only (default: \code{TRUE}). 
#' @param peaks_only Return pre-filtered metadata for peak data 
#' aligned to the GRCh38 human genome build. 
#' @param verbose Print messages. 
#' @param ... Additional metadata columns to filter. 
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
#' meta <- search_encode(assay = "chip-seq", 
#'                       biosample_name = "K562",
#'                       file_type = "peak",
#'                       assembly = "GRCh38",
#'                       organism = "Homo sapiens",
#'                       partial_match = TRUE)
search_encode <- function(## Filter args
                          assay=NULL,
                          accession=NULL,
                          file_accession=NULL,
                          file_type=NULL,
                          file_format=NULL,
                          output_category=NULL,
                          biosample_id=NULL,
                          biosample_type=NULL,
                          biosample_name=NULL,
                          organism=NULL,
                          dataset_type=NULL,
                          assembly=NULL,
                          genome_annotation=NULL,
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
        ## ENCODEexplorer seems to be deprecated, 
        ## but the metadata is still available on AnnotationHub 
        # and may be useful.
        # encode_meta <- dm[["AH75131"]] ## Curated version
        # dm <- AnnotationHub::query(ah, c("^ENCODE|wgENCODE"))
        # meta_all <- data.table::data.table(
        #     data.frame(AnnotationHub::mcols(dm)),
        #     keep.rownames = "ah_id"
        # )
        ## Large version 
        ## Already in data.table format. 
        ## No ah_ids because only the metadata is on AnnotationHub.
        meta <- ah[["AH75132"]] 
    } else{
        messager("Importing pre-filtered metadata via piggyback.",v=verbose)
        meta <- peaks_metadata_encode()
        drop_empty_cols(dt = meta,
                        verbose = verbose)
    } 
    #### Get arguments ####
    arg_list <- as.list(match.call(definition = search_encode))[-1]
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
