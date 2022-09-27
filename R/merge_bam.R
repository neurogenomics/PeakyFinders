#' Merge BAM files
#' 
#' If >1 BAM file provided, merges them into a single BAM file using 
#' \link[Rsamtools]{mergeBam}.
#' @returns Path to merged BAM file.
#' 
#' @export
#' @inheritParams pooled_peaks
#' @inheritParams Rsamtools::mergeBam
#' @examples
#' bam_files <- c(system.file("extdata", "querybins.bam", package="Rsamtools"),
#'                system.file("extdata", "revbins.bam", package="Rsamtools"))
#' bam_merged <- merge_bam(bam_files = bam_files)
merge_bam <- function(bam_files,
                      destination=tempfile(fileext = ".merged.bam"),
                      overwrite=TRUE, 
                      verbose=TRUE){
    bam_files <- unlist(bam_files)
    #### Merge bam ####
    if(length(bam_files)>1){
        messager("Merging",
                 formatC(length(bam_files),big.mark = ","),"BAM files.",
                 v=verbose)
        dir.create(dirname(destination),showWarnings = FALSE, recursive = TRUE) 
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
    return(merged)
}
