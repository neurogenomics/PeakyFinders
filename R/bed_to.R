#' Convert a BED file
#' 
#' Convert a BED file to any format supported by \link[rtracklayer]{export}.
#' @param ... Additional arguments passed to \link[rtracklayer]{export}.
#' @param files Paths to one or more BED files.
#' @param save_dir Path to save bigBed files.
#' @inheritParams fix_seqinfo
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
                   formats=c("bigwig","bigbed"),
                   save_dir=tempdir(),
                   verbose=TRUE,
                   ...){
    
    if(is.null(names(files))){
        names(files) <- make.unique(basename(files))
    }
    lapply(files,
           function(y){
               messager("Converting: ",basename(y),v=verbose)     
               gr <- import_peaks(ids = y)[[1]][[1]]
               #### Ensure there is a score col ####
               if(!"score" %in% names(GenomicRanges::mcols(gr))){
                   gr <- add_mcol(
                       gr = gr,
                       name = "score", 
                       value =  gr$total_signal)  
               }
               #### Ensure score col is between 0-1000 ####
               if(max(gr$score,na.rm = TRUE)>1000){
                   gr <- add_mcol(
                       gr = gr,
                       name = "score", 
                       value = gr$score/range(gr$score, na.rm = TRUE) * 1000) 
               }
               gr <- fix_seqinfo(gr = gr,
                                 build = build)
               lapply(stats::setNames(formats,
                                      formats), 
                      function(f){
                   outfile <- file.path(
                       save_dir,
                       gsub("\\.bed$",paste0(".",f),basename(y)))
                   dir.create(dirname(outfile), 
                              showWarnings = FALSE, recursive = TRUE) 
                   messager(paste0("Exporting to ",f,":"),outfile,v=verbose)
                   rtracklayer::export(object = gr, 
                                       con = outfile,
                                       format = f,
                                       ...) 
                   return(outfile)
               })  
       }) 
}
