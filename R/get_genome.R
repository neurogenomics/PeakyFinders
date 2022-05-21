#' Get genome 
#' 
#' Get a genome (or a subset of chromosomes from a genome) 
#' as a \link[GenomicRanges]{GRanges} object. This can be useful for 
#' querying whole-chromosomes at a time. 
#' @inheritParams regioneR::getGenome
#' @export
#' @importFrom regioneR filterChromosomes getGenome
#' @importFrom stringr str_sort
#' @importFrom GenomeInfoDb seqlevelsStyle
#' @examples 
#' gr <- get_genome(keep.chr=1:2)
get_genome <- function(keep.chr = paste0("chr",c(seq_len(22),"X","Y")),
                       genome = "hg19",
                       sort = TRUE,
                       style = "UCSC",
                       split_chromosomes = FALSE
                       ){ 
    #### Standardize chromosomes ####
    keep.chr <- paste0("chr",gsub("chr","",keep.chr))
    if(sort) { 
        keep.chr <- stringr::str_sort(keep.chr,numeric = TRUE) 
    }
    gr <- regioneR::filterChromosomes(
        A = regioneR::getGenome(genome = genome),
        keep.chr = keep.chr
    ) 
    #### Set chromosome style ####
    suppressMessages(suppressWarnings(
        GenomeInfoDb::seqlevelsStyle(gr) <- style
    ))
    #### Split by chromosome ####
    if(split_chromosomes){
        gr <- split_chromosomes_run(gr)
    }
    return(gr)
}
