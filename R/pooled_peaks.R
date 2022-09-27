#' Pooled peaks
#' 
#' Pool groups of BAM files into a smaller number of BAM files and call peaks
#' on each of them. 
#' @param bam_files One or more BAM files.
#' @param groups A character vector of the same length as \code{bam_files}
#' defining how to group files when calling consensus peaks.
#' @param method Method to call consensus peaks with:
#' \itemize{
#' \item{"MACSr:}{Call consensus peaks by merging groups of BAM files with 
#' \link[Rsamtools]{mergeBam} and then calling peaks with 
#' \link[MACSr]{callpeak}.
#' }
#' }
#' @returns \link[GenomicRanges]{GRangesList} object.
#' 
#' @export
#' @examples
#' bam_files <- example_bam()
#' peaks <- pooled_peaks(bam_files = bam_files)
pooled_peaks <- function(bam_files,
                         groups=names(bam_files),
                         save_dir=tempdir(),
                         method=c("MACSr","SEACR"),
                         verbose=TRUE,
                         ...){
    
    method <- tolower(method)[1]
    #### Check groups ####
    groups <- check_groups(files = bam_files, 
                           groups = groups) 
    #### Iterate over groups ####
    peaks_grouped <- lapply(groups, 
                            function(g){  
        bfiles <- bam_files[which(groups==g)] 
        messager("Computing consensus peaks for group:",g,
                 paste0("\n - ",length(bfiles)," file(s)."
                 )) 
        #### Call peaks ####
        if(method=="macsr"){  
            peaks <- pooled_peaks_macsr(bam_files=bfiles,
                                           save_dir=save_dir,
                                           g=g, 
                                           verbose=verbose,
                                           ...) 
        } else if(method=="seacr") {
            peaks <- pooled_peaks_seacr(bam_files=bfiles,
                                           save_dir=save_dir, 
                                           g=g, 
                                           verbose=verbose,
                                           ...)
        } else { 
            stopper("Method must be one of:",
                    paste("\n -",tolower(formals(pooled_peaks)$method),
                          collapse = ""))
        }
        #### Report ####
        messager("Identified",formatC(length(peaks),big.mark = ","),
                 "consensus peaks from",formatC(length(bfiles),big.mark = ","),
                 "peak files.",v=verbose)
        return(peaks)
    }) 
    #### Return ####
    if(length(peaks_grouped)==1) {
        return(peaks_grouped[[1]])
    } else {
        return(peaks_grouped) 
    } 
}
