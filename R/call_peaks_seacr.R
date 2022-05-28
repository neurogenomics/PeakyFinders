#' #' Call peaks: SEACR
#' #' 
#' #' Call peaks from a bedGraph or bigWig file using 
#' #' \href{https://github.com/FredHutch/SEACR}{SEACR} 
#' #' (Sparse Enrichment Analysis for CUT&RUN).  
#' #' @inheritParams call_peaks
#' #' 
#' #' @returns \link[GenomicRanges]{GRanges} or path to save peaks file.
#' #' 
#' #' @keywords internal
#' #' @importFrom rtracklayer import.bed import export.bedGraph 
#' #' @importFrom R.utils isGzipped gunzip 
#' #' @importFrom utils download.file
#' #' @importFrom data.table fread
#' #' @importFrom stats median 
#' call_peaks_macsr <- function(bedgraph_path,
#'                              exp,
#'                              ctrl,
#'                              norm,
#'                              outdir = tempdir(),
#'                              outputfile = "SEACR.peaks.bed", 
#'                              return_path = FALSE,
#'                              verbose=TRUE){ 
#'     #### Download ####
#'     bedgraph_path <- download_bedgraph(bedgraph_path = bedgraph_path)
#'     #### Convert to bedGraph ####
#'     bedgraph_path <- bigwig_to_bedgraph(path = bedgraph_path)
#'     #### Determine cutoff threshold autmatically ####
#'     dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
#'      
#'     # echoconda::find_packages(packages = "seacr")
#'     
#'     
#'     
#'     
#'     if(return_path) {
#'         return(out$outputs)
#'     } else {
#'         peaks <- rtracklayer::import.bed(out$outputs)
#'         messager(formatC(length(peaks),big.mark = ","),"peaks called.",
#'                  v=verbose)
#'         return(peaks)
#'     }
#' }
