#' Convert data.table to GRanges object
#'
#' Convert \link[data.table]{data.table}/\link[base]{data.frame} to a 
#' \link[GenomicRanges]{GRanges} object.
#' 
#' @param dat Data.
#' @param chrom_col Chromosome column name.
#' @param start_col Genomic start position column name.
#' @param end_col Genomic end position column name.
#' @param style GRanges style (e.g. "NCBI, "UCSC") 
#' set by \link[GenomeInfoDb]{seqlevelsStyle}.
#' @param verbose Print messages.
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
#' @family utils
#' @export
#' @importFrom GenomeInfoDb seqlevelsStyle
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @examples 
#' dat <- data.frame(GenomicRanges::GRanges("4:1-1000000"))
#' gr <- dt_to_granges(dat = dat)
dt_to_granges <- function(dat,
                          chrom_col = "seqnames",
                          start_col = "start",
                          end_col = "end",
                          style = "NCBI",
                          verbose = TRUE) { 
    if (is_granges(dat)) {
        messager("dat is already a GRanges object.", v = verbose)
        gr.snp <- dat
    } else {
        messager("Converting dat to GRanges object.", v = verbose) 
        gr.snp <- GenomicRanges::makeGRangesFromDataFrame(
            dat,
            seqnames.field = chrom_col,
            start.field = start_col,
            end.field = end_col,
            keep.extra.columns = TRUE
        )
    }
    suppressMessages(suppressWarnings(
        GenomeInfoDb::seqlevelsStyle(gr.snp) <- style
    ))
    return(gr.snp)
}
