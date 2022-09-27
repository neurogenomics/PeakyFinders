#' Call consensus peaks: \pkg{MACSr}
#' 
#' Call consensus peaks by merging groups of BAM files with 
#' \link[Rsamtools]{mergeBam} and then calling peaks with 
#' \link[MACSr]{callpeak}. 
#' @inheritParams pooled_peaks
#' @inheritParams Rsamtools::mergeBam
#' @inheritDotParams MACSr::callpeak
#' @returns \link[GenomicRanges]{GRanges} object.
#' 
#' @keywords internal
#' @importFrom Rsamtools mergeBam
#' @importFrom MACSr callpeak
pooled_peaks_macsr <- function(bam_files,
                               save_dir=tempdir(),
                               g=NULL,
                               overwrite=TRUE,
                               verbose=TRUE,
                               ...){ 
     
    bam_files <- unlist(bam_files)
    #### Merge bam ####
    if(length(bam_files)>1){
        messager("Merging",
                 formatC(length(bam_files),big.mark = ","),"BAM files.",
                 v=verbose)
        dir.create(save_dir,showWarnings = FALSE, recursive = TRUE)
        destination <- file.path(save_dir,paste(g,"merged.bam",sep="."))
        t1 <- Sys.time() 
        merged <- Rsamtools::mergeBam(files = bam_files,
                                      destination = destination,
                                      overwrite = overwrite)
        report_time(start = t1, 
                    prefix = "Merging BAM:",
                    verbose = verbose)
    } else {
        messager("Only 1 file in group. Skipping BAM merging step.",v=verbose)
        merged <- bam_files
    } 
    #### Call peaks #### 
    messager("Calling consensus peaks with: MACSr",v=verbose)
    t1b <- Sys.time()
    out <- MACSr::callpeak(tfile = merged,
                           ...)
    report_time(start = t1b, 
                prefix = "MACSr peak calling:",
                verbose = verbose)
    return(out)
}
