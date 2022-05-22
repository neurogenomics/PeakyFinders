#' Get ENCODE peaks metadata
#' 
#' A subset of the ENCODE metadata that includes links to all
#'  human epigenomic peaks data. 
#' @param save_dir Directory to cache file in.
#' @source 
#' \code{
#' ### Get peak data only #####
#' meta <- search_encode(file_type = c("^bed narrowpeak","^bed broadpeak"),
#'                       assembly = "GRCh38",
#'                       organism = "Homo sapiens",
#'                       partial_match = TRUE)
#' meta <- subset(meta[(!file_type %in% c("vcf","hic")),])
#' PeakyFinders:::drop_empty_cols(meta)
#' tmp <- "~/Downloads/encode_peaks_metadata.tsv.gz"
#' data.table::fwrite(meta, file = tmp, sep="\t")
# piggyback::pb_new_release(repo = "neurogenomics/PeakyFinders",
#                           tag="latest")
# piggyback::pb_upload(file = tmp,
#                      repo = "neurogenomics/PeakyFinders",
#                      overwrite = TRUE)
#' }
#' @inheritParams piggyback::pb_download
#' 
#' @returns \link[data.table]{data.table}. 
#' 
#' @export
#' @importFrom tools R_user_dir
#' @examples 
#' meta <- peaks_metadata_encode()
peaks_metadata_encode <- function(save_dir=tools::R_user_dir(
    package = "PeakyFinders", 
    which = "cache"),
    overwrite = FALSE
    ){
    requireNamespace("piggyback")
    path <- file.path(save_dir, "encode_peaks_metadata.tsv.gz")
    if((!file.exists(path)) | isTRUE(overwrite)){
        dir.create(save_dir,showWarnings = FALSE, recursive = TRUE)
        piggyback::pb_download(file = basename(path), 
                               repo = "neurogenomics/PeakyFinders",
                               dest = save_dir,
                               overwrite = overwrite)
    }
    meta <- data.table::fread(path)
    return(meta)
}
