#' Pooled peaks
#' 
#' Pool groups of BAM files into a smaller number of BAM files and call peaks
#' on each of them. 
#' @param bam_files One or more BAM files.
#' @param groups A character vector of the same length as \code{bam_files}
#' defining how to group files when calling consensus peaks.
#' @param method Method to call consensus peaks with:
#' \itemize{
#' \item{"MACSr : }{Call consensus peaks by merging groups of BAM files with 
#' \link[Rsamtools]{mergeBam} and then calling peaks with 
#' \link[PeakyFinders]{call_peaks_macsr}.
#' }
#' \item{"SEACR : }{Call consensus peaks by merging groups of BAM files with 
#' \link[Rsamtools]{mergeBam} and then calling peaks with 
#' \link[PeakyFinders]{call_peaks_seacr}.
#' }
#' }
#' @param outdir Directory to save results to.
#' @param ... Additional argument passed to either 
#' \link[PeakyFinders]{call_peaks_macsr} or
#' \link[PeakyFinders]{call_peaks_seacr}, depending on \code{method}. 
#' @inheritParams call_peaks
#' @returns \link[GenomicRanges]{GRangesList} object.
#' 
#' @export
#' @examples
#' bam_files <- example_bam()
#' peaks <- pooled_peaks(bam_files = bam_files)
pooled_peaks <- function(bam_files,
                         groups=names(bam_files),
                         outdir=tempdir(),
                         cutoff=NULL,
                         method=c("MACSr",
                                  "SEACR"),
                         verbose=TRUE,
                         ...){
    
    method <- tolower(method)[1]
    #### Check groups ####
    groups <- check_groups(files = bam_files, 
                           groups = groups) 
    #### Iterate over groups ####
    peaks_grouped <- lapply(unique(groups), 
                            function(g){  
        #### Subset files ####
        if(g=="all"){
            bfiles <- bam_files
        } else {
            bfiles <- bam_files[which(groups==g)] 
        }
        messager("Computing consensus peaks for group:",g,
                 paste0("[",length(bfiles)," file(s)","]"),
                 v=verbose) 
        #### Call peaks ####
        if(method=="macsr"){  
            peaks <- pooled_peaks_macsr(bam_files=bfiles,
                                        outdir=outdir,
                                        g=g, 
                                        cutoff=cutoff,
                                        verbose=verbose,
                                        ...) 
        } else if(method=="seacr") {
            peaks <- pooled_peaks_seacr(bam_files=bfiles,
                                        outdir=outdir, 
                                        g=g, 
                                        control=cutoff,
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
