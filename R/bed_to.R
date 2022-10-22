#' Convert a BED file
#' 
#' Convert a BED file to any format supported by \link[rtracklayer]{export}.
#' @param ... Additional arguments passed to \link[rtracklayer]{export}.
#' @param files Paths to one or more BED files.
#' @param save_dir Path to save bigBed files.
#' @inheritParams fix_seqinfo
#' @inheritParams filter_chrom
#' @returns Converted file paths.
#' 
#' @export
#' @importFrom stats setNames
#' @importFrom rtracklayer export
#' @importFrom GenomicRanges mcols
#' @examples 
#' files <- system.file("tests","test.bed",package = "rtracklayer")
#' out <- bed_to(files=files)
bed_to <- function(files,
                   build = "hg19",
                   keep_chr = NULL,
                   formats=c("bigwig","bigbed"),
                   save_dir=tempdir(),
                   verbose=TRUE,
                   ...){
    
    requireNamespace("rtracklayer")
    
    if(is.null(names(files))){
        names(files) <- make.unique(basename(files))
    }
    lapply(files,
           function(y){
               t1 <- Sys.time()
               messager("Converting: ",basename(y),v=verbose)     
               gr <- import_peaks(ids = y, 
                                  save_path = NULL,
                                  verbose = verbose)[[1]][[1]]
               #### Ensure there is a score col ####
               if(!"score" %in% names(GenomicRanges::mcols(gr)) &&
                   "total_signal" %in% names(GenomicRanges::mcols(gr)) ){
                   gr <- add_mcol(
                       gr = gr,
                       name = "score", 
                       value =  gr$total_signal,
                       verbose = verbose)  
               }
               if(!"score" %in% names(GenomicRanges::mcols(gr))){
                   messager("WARNING: Could not find score column.",
                            "Returning NULL.",v=verbose)
                   return(NULL)
               }
               #### Remove NAs #####
               gr <- dropna_granges(gr = gr, 
                                    cols = "score")
               #### Ensure score col is between 0-1000 ####
               if(max(gr$score,na.rm = TRUE)>1000){
                   gr <- add_mcol(
                       gr = gr,
                       name = "score", 
                       value = gr$score/range(gr$score, na.rm = TRUE)[2] * 1000)  
               }
               gr <- filter_chrom(grlist = gr,
                                  keep_chr = keep_chr,
                                  verbose = verbose)
               gr <- fix_seqinfo(gr = gr,
                                 build = build,
                                 verbose = verbose)
               lapply(stats::setNames(formats,
                                      formats), 
                      function(f){
                   outfile <- file.path(
                       save_dir,
                       gsub("\\.bed$",paste0(".",f),basename(y)))
                   dir.create(dirname(outfile), 
                              showWarnings = FALSE, recursive = TRUE) 
                   messager(paste0("Exporting to ",shQuote(f),":"),outfile,
                            v=verbose)
                   rtracklayer::export(object = gr, 
                                       con = outfile,
                                       format = f,
                                       ...) 
                   report_time(start = t1,
                               prefix = paste("Conversion to",shQuote(f)),
                               verbose = verbose)
                   return(outfile)
               })  
       }) 
}
