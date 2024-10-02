#' Convert a BAM file
#' 
#' Convert a BAM file to any format supported by \code{rtracklayer::export}.
#' @inheritParams pooled_peaks
#' @inheritParams bed_to
#' @inheritParams IRanges::resize 
#' @inheritDotParams  rtracklayer::export.bw
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
        t1 <- Sys.time()
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
        rpm <- methods::as(rpm,"SimpleRleList")
        #### export rpm to... ####
        lapply(stats::setNames(formats,
                               formats), 
               function(f){
            outfile <- paste(gsub("\\.bam", "", file),f,sep=".")
            if(!is.null(outdir)) {
                outfile <- file.path(outdir,basename(outfile))
            }
            messager(paste0("Exporting to ",shQuote(f),":"),outfile,
                     v=verbose)
            rtracklayer::export(object = rpm, 
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
