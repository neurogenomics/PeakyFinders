#' Construct searches
#' 
#' Construct a named list to be used to identify matching file types
#' based on their path names.
#' @param searches Named list of regex queries.
#' @param keys Keys to subset the \code{searches} with by their
#' names. 
#' 
#' @returns Named list. 
#' 
#' @export
#' @examples 
#' searches <- construct_searches()
construct_searches <- function(searches=NULL,
                               keys=NULL){
    if(is.null(searches)){
        searches = list(
            narrowPeak="narrowpeak",
            broadPeak="broadpeak",
            seacr="peaks.stringent.bed|peaks.relaxed.bed|SEACR.bed$",
            genericPeak="peak|bed.gz$|.bed",
            bedGraph="bedgraph|graph.gz|bdg.gz",
            bigWig="bigwig|bw$",
            bigBed="bigbed"
        )
    } 
    #### Make casing irrelevant ####
    names(searches) <- tolower(names(searches))
    if(!is.null(keys)){ 
        searches <- searches[
            grepl(paste(keys,collapse = "|"),names(searches),
                  ignore.case = TRUE)
        ]
    }
    if(length(searches)==0){
        stop("No valid searches were produced.")
    }
    return(searches)
}
