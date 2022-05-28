#' Call peaks: MACSr
#' 
#' Call peaks from a bedGraph or bigWig file using 
#' \pkg{MACS3}/\link[MACSr]{MACSr}.
#' By default, it automatically infers a reasonable 
#' \code{cutoff} threshold as well.\cr
#' \emph{Note : } 
#' \pkg{MACS3}/\link[MACSr]{MACSr} is not currently compatible with Windows 
#' (see \href{https://github.com/macs3-project/MACSr/issues/13}{here}
#'  for details). 
#' @inheritParams call_peaks
#' 
#' @returns \link[GenomicRanges]{GRanges} or path to save peaks file.
#' 
#' @keywords internal
#' @importFrom rtracklayer import.bed import export.bedGraph 
#' @importFrom R.utils isGzipped gunzip 
#' @importFrom utils download.file
#' @importFrom data.table fread
#' @importFrom stats median
#' @importFrom MACSr bdgpeakcall
call_peaks_macsr <- function(bedgraph_path,
                             cutoff = NULL,
                             minlen = 200L,
                             maxgap = 30L,
                             call_summits = TRUE,
                             trackline = TRUE,
                             log = TRUE,
                             outdir = tempdir(),
                             outputfile = "MACSr.peaks.bed", 
                             return_path = FALSE,
                             verbose=TRUE){
    if(.Platform$OS.type=="windows"){
        stopper("MACS3/MACSr is not currently compatible with Windows.")
    }
    #### Download ####
    bedgraph_path <- download_bedgraph(bedgraph_path = bedgraph_path)
    #### Convert to bedGraph ####
    bedgraph_path <- bigwig_to_bedgraph(path = bedgraph_path)
    #### Determine cutoff threshold autmatically ####
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    if(is.null(cutoff)){
        messager("Analyzing cutoff thresholds.",v=verbose)
        out_cutoff <- MACSr::bdgpeakcall(ifile = bedgraph_path, 
                                         outdir = outdir,
                                         cutoff_analysis = TRUE, 
                                         outputfile = gsub(
                                             ".bed$",".cutoff_analysis.txt",
                                             outputfile)
        )
        cutoffs <- data.table::fread(out_cutoff$outputs)
        cutoff <- stats::median(cutoffs$pscore)
        # qplot(as.factor(cutoffs$pscore), cutoffs$avelpeak, geom="violin")
    } 
    #### Call peaks #####
    messager("Calling peaks.",v=verbose)
    out <- MACSr::bdgpeakcall(ifile = bedgraph_path, 
                              cutoff = cutoff, 
                              call_summits = call_summits,
                              trackline = trackline,
                              log = log,
                              outdir = outdir,
                              outputfile = outputfile)
    if(return_path) {
        return(out$outputs)
    } else {
        peaks <- rtracklayer::import.bed(out$outputs)
        messager(formatC(length(peaks),big.mark = ","),"peaks called.",
                 v=verbose)
        return(peaks)
    }
}
