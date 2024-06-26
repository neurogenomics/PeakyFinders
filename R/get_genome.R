#' Get genome 
#' 
#' Get a genome (or a subset of chromosomes from a genome) 
#' as a \link[GenomicRanges]{GRanges} object. This can be useful for 
#' querying whole-chromosomes at a time. 
#' @param sort Whether to sort the \link[GenomicRanges]{GRanges}
#' by chromosomes. 
#' @param split_chromosomes Whether to split the 
#' \link[GenomicRanges]{GRanges} by chromosome (as a named list). 
#' @inheritParams liftover_grlist
#' @inheritParams regioneR::filterChromosomes
#' @inheritParams regioneR::getGenome
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
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
    if(!is.null(keep.chr)){
        keep.chr <- paste0("chr",gsub("chr","",keep.chr))
        if(isTRUE(sort)) { 
            keep.chr <- stringr::str_sort(keep.chr,
                                          numeric = TRUE) 
        }
    }
    genome <- translate_genome(genome = genome,
                               style = "UCSC")
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
