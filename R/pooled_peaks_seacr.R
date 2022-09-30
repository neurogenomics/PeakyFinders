#' Call consensus peaks: \pkg{SEACR}
#' 
#' Call consensus peaks by merging groups of BAM files with 
#' \link[Rsamtools]{mergeBam} and then calling peaks with 
#' \link[PeakyFinders]{call_peaks_seacr}. 
#' @inheritParams pooled_peaks
#' @inheritDotParams call_peaks_seacr
#' @returns \link[GenomicRanges]{GRanges} object.
#' 
#' @keywords internal
#' @importFrom Rsamtools mergeBam
#' @importFrom MACSr callpeak
pooled_peaks_seacr <- function(bam_files,
                               outdir=tempdir(), 
                               g=NULL,
                               overwrite=TRUE,
                               nThread=1,
                               verbose=TRUE,
                               ...){ 
    
    destination <- gsub("\\.+",".",
                        file.path(outdir,paste("pooled",g,"bam",sep=".")))
    merged <- merge_bam(bam_files=bam_files,
                        destination=destination,
                        overwrite=overwrite, 
                        verbose=verbose)
    bedgraph_path <- convert_bam(bam_files = merged, 
                                 formats = "bedGraph",
                                 outdir = outdir,
                                 verbose = verbose)[[1]][[1]]
    #### Call peaks #### 
    messager("Calling pooled consensus peaks with: SEACR",v=verbose) 
    outputfile <- gsub("\\.+",".",paste("poooled_peaks",g,sep="."))
    peaks <- call_peaks_seacr(bedgraph_path = bedgraph_path, 
                              outdir = outdir,
                              outputfile = outputfile, 
                              return_path = FALSE,
                              verbose = verbose,
                              ...) 
    return(peaks)
}
