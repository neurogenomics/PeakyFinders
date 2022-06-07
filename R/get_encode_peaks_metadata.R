#' Get ENCODE peaks metadata
#' 
#' Get a subset of the ENCODE metadata for epigenomic peaks in human data.
#' @keywords internal
#' @source 
#' \code{
#' ### Get peak data only #####
#' meta <- search_encode(file_type = "peak",
#'                       assembly = "GRCh38",
#'                       organism = "Homo sapiens",
#'                       partial_match = TRUE)
#' for(col in names(meta)){
#'     if(all(is.na(meta[[col]]))){
#'         messager("Removing:",col)
#'         meta[[col]] <- NULL
#'     }
#' }
#' tmp <- "~/Downloads/encode_peaks_metadata.tsv.gz"
#' data.table::fwrite(meta, file = tmp, sep="\t")
#' piggyback::pb_new_release(repo = "neurogenomics/PeakyFinders",
#'                           tag="latest")
#' piggyback::pb_upload(file = tmp,
#'                      repo = "neurogenomics/PeakyFinders",
#'                      overwrite = TRUE)
#' }
#' @inheritParams piggyback::pb_download
#' @importFrom tools R_user_dir
get_encode_peaks_metadata <- function(save_dir=tools::R_user_dir(
    package = "PeakyFinders", 
    which = "cache"),
    overwrite = FALSE
    ){
    requireNamespace("piggyback")
    path <- file.path(save_dir, "encode_peaks_metadata.tsv.gz")
    if(!file.exists(path)){
        dir.create(save_dir,showWarnings = FALSE, recursive = TRUE)
        piggyback::pb_download(file = basename(path), 
                               repo = "neurogenomics/PeakyFinders",
                               dest = save_dir,
                               overwrite = overwrite)
    }
    meta <- data.table::fread(path)
    return(meta)
}
