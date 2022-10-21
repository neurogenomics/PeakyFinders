#' Convert a BAM file
#' 
#' Convert a BAM file to any format supported by \link[rtracklayer]{export}.
#' @param ... Additional arguments passed to \link[rtracklayer]{export}.
#' @inheritParams pooled_peaks
#' @inheritParams GenomicRanges::resize 
#' @returns Converted file paths.
#' 
#' @export
#' @importFrom stats setNames
#' @importFrom rtracklayer import export
#' @importFrom GenomicRanges resize trim coverage
#' @examples
#' bam_files <- c(f1=system.file("extdata","querybins.bam",package="Rsamtools"),
#'                f2=system.file("extdata","revbins.bam",package="Rsamtools"))
#' converted_files <- bam_to(bam_files = bam_files)               
bam_to <- function(bam_files,
                   formats=c("bedGraph","bigWig"),
                   width=150,
                   outdir=NULL,
                   verbose=TRUE,
                   ...){
    
    #### Name files ####
    if(is.null(names(bam_files))){
        names(bam_files) <- bam_files
    }
    lapply(bam_files, 
           function(file){
        messager("Processing:",file,v=verbose)              
        #### Import BAM ####
        bam <- rtracklayer::import(file, format="bam") 
        #### Convert to GRanges ####
        bam <- methods::as(bam,"GRanges") 
        messager("Extending reads.",v=verbose)
        if(!is.null(width) && width>0){
            bam <- GenomicRanges::resize(bam, width=width)
        } 
        bam <- GenomicRanges::trim(bam) 
        totalReads <- length(bam) 
        messager("Computing coverage.",v=verbose)
        #### get coverage ####
        cov <- GenomicRanges::coverage(bam)
        rpm <- lapply(cov, function(x) signif(10^6 * x/totalReads,3))
        rpm <- as(rpm,"SimpleRleList")
        #### export rpm to... ####
        lapply(stats::setNames(formats,
                               formats), 
               function(f){
            outfile <- paste(gsub("\\.bam", "", file),f,sep=".")
            if(!is.null(outdir)) {
                outfile <- file.path(outdir,basename(outfile))
            }
            messager(paste0("Exporting to ",f,":"),outfile,v=verbose)
            rtracklayer::export(object = rpm, 
                                con = outfile, 
                                format = f,
                                ...)
            return(outfile)
        })  
    })
}
