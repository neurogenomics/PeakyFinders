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
#' @param method Method to call peaks with:
#' \itemize{
#' \item{"MACSr" : }{Uses \href{https://github.com/macs3-project/MACS}{MACS3} 
#' via \link[MACSr]{bdgpeakcall}.}
#' \item{"SEACR" : }{Uses \href{https://github.com/FredHutch/SEACR}{SEACR} 
#' via \link[echoconda]{find_packages}.}
#' }
#' @param cutoff 
#' \itemize{
#' \item{when \code{method="MACSr"} : }{
#' Passed to \code{cutoff} argument.
#' Cutoff depends on which method you used for score track.
#'  If the file contains pvalue scores from MACS3, score 5 means pvalue 1e-5.
#'  If \code{NULL}, a reasonable \code{cutoff} value will be inferred 
#'  through a \code{cutoff_analysis}. 
#' }
#' \item{when \code{method="SEACR"} : }{
#' Passed to \code{control} argument.
#' Control (IgG) data bedgraph file to generate an empirical
#' threshold for peak calling.
#' Alternatively, a numeric threshold n between 0 and 1 returns the top n
#' fraction of peaks based on total signal within peaks 
#' (default: \code{0.05}).
#' }
#' } 
#' @param outdir Directory to store \code{cutoff_analysis} 
#' report and peak file in.
#' @param outputfile Name of the peak output file (stored in BED format).
#' @param return_path Whether to return the path to the saved peak file, 
#' or the peak data itself as a \link[GenomicRanges]{GRanges} object.
#' @param verbose Print messages.
#' @inheritParams MACSr::bdgpeakcall
#' @inheritParams call_peaks_seacr
#' 
#' @returns \link[GenomicRanges]{GRanges} or path to save peaks file.
#' 
#' @export
#' @examples 
#' files <- example_bg_bw()
#' peaks <- PeakyFinders::call_peaks(bedgraph_path = files$bedgraph,
#'                                   method="SEACR")
call_peaks <- function(#### Shared args ####
                       bedgraph_path,
                       method = c("MACSr",
                                  "SEACR"),
                       cutoff = NULL,
                       #### MACSr args ####
                       minlen = 200L,
                       maxgap = 30L,
                       call_summits = TRUE,
                       trackline = TRUE,
                       log = TRUE,
                       #### SEACR args ####
                       norm = TRUE,
                       stringent = TRUE,
                       outdir = tempdir(),
                       outputfile = paste0(method[[1]],
                                           ".peaks.bed"), 
                       return_path = FALSE,
                       nThread = 1,
                       verbose = TRUE){
    
    method <- tolower(method)[1]
    if(method=="macsr"){
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
    } else if(method=="seacr") {
        peaks <- call_peaks_seacr(bedgraph_path = bedgraph_path, 
                                  control = cutoff,
                                  norm = norm,
                                  stringent = stringent,
                                  outdir = outdir,
                                  outputfile = outputfile, 
                                  return_path = return_path,
                                  nThread = nThread,
                                  verbose = verbose)
    } else{
        opts <- formals(call_peaks)$method
        stopper("method must be one of:",
                paste("\n -",paste0("'",opts,"'"),collapse = ""))
    }
   return(peaks)
}
