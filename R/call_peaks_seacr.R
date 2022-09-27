#' Call peaks: SEACR
#'
#' Call peaks from a bedGraph or bigWig file using
#' \href{https://github.com/FredHutch/SEACR}{SEACR}
#' (Sparse Enrichment Analysis for CUT&RUN).
#' @param bedgraph_path Field 1: Target data bedgraph file in 
#' \href{https://genome.ucsc.edu/goldenpath/help/bedgraph.html}{
#' UCSC bedgraph format} that omits regions containing 0 signal.
#' @param control Control (IgG) data bedgraph file to generate an empirical
#' threshold for peak calling.
#' Alternatively, a numeric threshold n between 0 and 1 returns the top n
#' fraction of peaks based on total signal within peaks 
#' (default: \code{0.05}).
#' @param norm Field 3: "norm" (default: \code{norm=TRUE}) 
#' denotes normalization of control 
#' to target data, "non" (\code{norm=FALSE}) skips this behavior.
#' "norm" is recommended unless experimental and control data are already 
#' rigorously normalized to each other (e.g. via spike-in).
#' @param stringent Field 4:"relaxed" (\code{stringent=FALSE})
#' uses a total signal 
#' threshold between the knee and peak of the total signal curve,
#' and corresponds to the “relaxed” mode described in the text, 
#' whereas “stringent” (default: \code{stringent=TRUE}) 
#' uses the peak of the curve, and corresponds to “stringent” mode. 
#' @inheritParams import_peaks 
#' @inheritParams call_peaks
#' @returns \link[GenomicRanges]{GRanges} or path to save peaks file.
#' @source \href{https://github.com/FredHutch/SEACR/issues/54}{
#' GitHub Issue: illegal byte sequence}
#'
#' @keywords internal
#' @importFrom rtracklayer import.bed import export.bedGraph
#' @importFrom R.utils isGzipped gunzip
#' @importFrom utils download.file
#' @importFrom data.table fread
#' @importFrom stats median
#' @importFrom echoconda cmd_print
call_peaks_seacr <- function(bedgraph_path, 
                             control=0.05,
                             norm=TRUE,
                             stringent=TRUE,
                             outdir=tempdir(),
                             outputfile = basename(
                                 tempfile(pattern = bedgraph_path,
                                          fileext = ".SEACR_peaks")),
                             return_path=FALSE,
                             nThread=1,
                             verbose=TRUE){
    #### Check control arg ####
    if(is.null(control)){
        default <- formals(call_peaks_seacr)$control
        messager("control=NULL. Setting to default value:",default,
                 v=verbose)
        control <- default
    }
    #### Download ####
    bedgraph_path <- download_bedgraph(bedgraph_path = bedgraph_path)
    #### Convert to bedGraph ####
    bedgraph_path <- bigwig_to_bedgraph(path = bedgraph_path,
                                        verbose = verbose) 
    #### Set up paths ####
    output_prefix <- file.path(outdir,gsub("\\.bed$","",outputfile))
    dir.create(dirname(output_prefix), showWarnings = FALSE, recursive = TRUE)
    seacr <- find_executable_seacr(verbose = verbose)
    #### Call peaks ####
    messager("Calling peaks with:","SEACR",v=verbose)
    t1 <- Sys.time()
    cmd <- paste(seacr[["SEACR_1.3.sh"]],
                 bedgraph_path,
                 control,
                 if(isTRUE(norm)) "norm" else "non",
                 if(isTRUE(stringent)) "stringent" else "relaxed",
                 output_prefix)
    echoconda::cmd_print(cmd,
                         verbose = verbose)
    out <- system(cmd, intern = TRUE)
    #### Report ####
    messager(paste(out,collapse = "\n"),v=verbose)
    report_time(start = t1, 
                prefix = "SEACR peak calling:",
                verbose = verbose)
    #### Gather results #### 
    bed_files <- list.files(dirname(output_prefix),
                            pattern = "\\.bed$", 
                            full.names = TRUE) 
    #### Return ####
    if(length(bed_files)==0){
        stopper("0 BED files produced.")
    } else {
        messager(formatC(length(bed_files),big.mark = ","),
                 "BED file(s) produced:",
                 paste("\n -",bed_files,collapse = ""),
                 v = verbose)
        if(isTRUE(return_path)) {
            return(bed_files)
        } else {
            peaks <- import_peaks_seacr(paths = bed_files, 
                                        nThread = nThread, 
                                        verbose = verbose)
            return(peaks)
        } 
    } 
}
