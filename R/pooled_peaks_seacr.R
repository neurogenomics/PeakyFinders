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
                               save_dir=tempdir(), 
                               g=NULL,
                               overwrite=TRUE,
                               nThread=1,
                               verbose=TRUE,
                               ...){ 
    
    destination <- gsub("\\.+",".",
                        file.path(save_dir,paste("merged",g,"bam",sep=".")))
    merged <- merge_bam(bam_files=bam_files,
                        destination=destination,
                        overwrite=overwrite, 
                        verbose=verbose)
    bedgraph_path <- convert_bam(bam_files = merged, 
                                 formats = "bedGraph", 
                                 verbose = verbose)[[1]][[1]]
    #### Call peaks #### 
    messager("Calling consensus peaks with: SEACR",v=verbose) 
    outputfile <- gsub("\\.+",".",
                       file.path(save_dir,paste("consensus",g,sep=".")))
    peaks <- call_peaks_seacr(bedgraph_path = bedgraph_path, 
                              outdir = save_dir,
                              outputfile = outputfile, 
                              return_path = FALSE,
                              verbose = verbose,
                              ...) 
    return(peaks)
}
