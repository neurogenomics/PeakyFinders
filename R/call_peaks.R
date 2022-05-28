#' Call peaks
#' 
#' Call peaks from a bedGraph or bigWig file using multiple methods.
#' By default, it automatically infers a reasonable 
#' \code{cutoff} threshold as well.\cr
#' \emph{Note : } 
#' \pkg{MACS3}/\pkg{MACSr} is not currently compatible with Windows 
#' (see \href{https://github.com/macs3-project/MACSr/issues/13}{here}
#'  for details). 
#' 
#' @param bedgraph_path Path to bedGraph file. 
#' Can instead provide a bigWig file, 
#' but this will first be converted to bedGraph format, 
#' which can take some time if trying to convert data from across the entire
#' genome.
#' @param call_peaks_method Method to call peaks with:
#' \itemize{
#' \item{"MACSr" : }{Uses \href{https://github.com/macs3-project/MACS}{MACS3} 
#' via \link[MACSr]{bdgpeakcall}.}
#' }
#' @param cutoff Cutoff depends on which method you used for score track.
#'  If the file contains pvalue scores from MACS3, score 5 means pvalue 1e-5.
#'  If \code{NULL}, a reasonable \code{cutoff} value will be inferred 
#'  through a \code{cutoff_analysis}. 
#' @param outdir Directory to store \code{cutoff_analysis} 
#' report and peak file in.
#' @param outputfile Name of the peak output file (stored in BED format).
#' @param return_path Whether to return the path to the saved peak file, 
#' or the peak data itself as a \link[GenomicRanges]{GRanges} object.
#' @param verbose Print messages.
#' @inheritParams MACSr::bdgpeakcall
#' 
#' @returns \link[GenomicRanges]{GRanges} or path to save peaks file.
#' 
#' @export
#' @examples
#' #### Get bedGraph subset ####
#' ## Normally, you'd call peaks on the entire chromosome, 
#' ## or even the whole genome. But for demo purposes we'll just use one locus.
#' gsm <- "GSM4703766" 
#' links <- PeakyFinders:::get_geo_links(gsm = gsm) 
#' query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
#' gr <- rtracklayer::import(con = links$bedgraph, 
#'                           which = query_granges)
#' tmp <- tempfile(fileext = ".bedgraph")
#' rtracklayer::export.bedGraph(object = gr, con = tmp)
#' 
#' #### Call peaks #### 
#' if(.Platform$OS.type!="windows"){
#' peaks <- PeakyFinders::call_peaks(bedgraph_path = tmp)
#' }
call_peaks <- function(bedgraph_path,
                       call_peaks_method = "MACSr",
                       cutoff = NULL,
                       minlen = 200L,
                       maxgap = 30L,
                       call_summits = TRUE,
                       trackline = TRUE,
                       log = TRUE,
                       outdir = tempdir(),
                       outputfile = "MACSr.peaks.bed", 
                       return_path = FALSE,
                       verbose = TRUE){
    call_peaks_method <- tolower(call_peaks_method)[1]
    
    if(call_peaks_method=="macsr"){
        peaks <- call_peaks_macsr(bedgraph_path = bedgraph_path, 
                                  cutoff = cutoff,
                                  minlen = minlen,
                                  maxgap = maxgap,
                                  call_summits = call_summits,
                                  trackline = trackline,
                                  log = log, 
                                  outdir = outdir,
                                  outputfile = outputfile,
                                  return_path = return_path,
                                  verbose = verbose)
    } else {
        opts <- formals(call_peaks)$call_peaks_method
        stopper("call_peaks_method must be one of:",
                paste("\n -",paste0("'",opts,"'"),collapse = ""))
    }
   return(peaks)
}
