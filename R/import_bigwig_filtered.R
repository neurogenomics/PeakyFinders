#' Import filtered bigwig
#'
#' Import a subset of a bigwig file
#' based on the coordinates in a \link[GenomicRanges]{GRanges} 
#' object (\code{gr.query_dat}).
#' @param bw.file Path to a bigwig file.
#' @param query_granges \link[GenomicRanges]{GRanges}
#'  object to query the bigWig file with.
#' @param full_data Whether to return the actual read ranges
#'  (\code{full_data=TRUE}),
#' or just the "score" column which summarizes the height of
#' the aggregated reads across the genome (\code{full_data=TRUE}).
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
#' @keywords internal
#' @importFrom GenomicRanges mcols start end
#' @importFrom rtracklayer import.bw
import_bigwig_filtered <- function(bw.file,
                                   query_granges,
                                   full_data = TRUE) {
    #### Get all ranges within min/max ####
    ## Otherwise, just use the score for the exact values.
    if (full_data) {
        query_granges <- condense_granges(gr = query_granges) 
    } 
    # query_granges <- get_genome(keep.chr = "chr22", style = "NCBI")
    # query_granges <- rtracklayer::BigWigSelection(ranges = query_granges,
    #                                               colnames = "score")
    # bw.file <- paste0(
    #     "https://www.encodeproject.org",
    #     "/files/ENCFF103JOZ/@@download/ENCFF103JOZ.bigWig"
    # )
    #### Try reticulate + pyBigWig ####
    # echoconda::activate_env(conda_env = "epiprepare")
    # echoconda::which_env()
    # pyBigWig <- reticulate::import("pyBigWig")
    # bw <- pyBigWig$open(bw.file)
    # bw$isBigWig()
    # bw$chroms()
    # bw$header()
    # dat <- bw$intervals("chr22")
    
    # Sys.setlocale(locale="C") 
    # tmp <- tempfile()
    # download.file(bw.file, tmp)
    
    # bw.filt <- plyranges::read_bigwig(file = bw.file,
    #                                   genome_info = query_granges)
    bw.filt <- rtracklayer::import(
        con = bw.file,
        selection = query_granges
    ) 
    # plot(x = start(bw.filt), y=bw.filt$score)
    return(bw.filt)
}
