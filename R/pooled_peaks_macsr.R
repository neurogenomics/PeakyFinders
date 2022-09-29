#' Call consensus peaks: \pkg{MACSr}
#' 
#' Call consensus peaks by merging groups of BAM files with 
#' \link[Rsamtools]{mergeBam} and then calling peaks with 
#' \link[MACSr]{callpeak}. 
#' @inheritParams pooled_peaks
#' @inheritParams Rsamtools::mergeBam
#' @inheritParams MACSr::callpeak 
#' @inheritDotParams MACSr::callpeak
#' @returns \link[GenomicRanges]{GRanges} object.
#' 
#' @keywords internal
#' @importFrom Rsamtools mergeBam
#' @importFrom MACSr callpeak
pooled_peaks_macsr <- function(bam_files,
                               outdir=tempdir(),
                               g=NULL,
                               overwrite=TRUE,
                               verbose=TRUE,
                               ...){ 
     
    #### Merge bam ####
    destination <- gsub("\\.+",".",
                        file.path(outdir,paste("merged",g,"bam",sep=".")))
    merged <- merge_bam(bam_files=bam_files,
                        destination=destination,
                        overwrite=overwrite, 
                        verbose=verbose)
    #### Call peaks #### 
    messager("Calling consensus peaks with: MACSr",v=verbose)
    t1b <- Sys.time()
    out <- MACSr::callpeak(tfile = merged,
                           outdir = outdir,
                           ...)
    report_time(start = t1b, 
                prefix = "MACSr peak calling:",
                verbose = verbose)
    return(out)
}
