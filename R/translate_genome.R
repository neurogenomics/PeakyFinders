#' Translate genome
#' 
#' Translate the name of a genome build from one format to another.
#' @param omit_subversion Omit any subversion suffixes after the ".". 
#' @inheritParams GenomeInfoDb::mapGenomeBuilds
#' 
#' @returns Standardized genome build name as a character string. 
#' 
#' @export
#' @importFrom GenomeInfoDb mapGenomeBuilds
#' @importFrom utils tail
#' @examples 
#' genome <- translate_genome(genome="hg38", style="Ensembl")
translate_genome <- function(genome,
                             style=c("UCSC", "Ensembl","NCBI"),
                             omit_subversion=TRUE){
    if(length(style)>1){
        style <- style[1]
        messager("WARNING: >1 style supplied. Only using the first:",style)
    }
    #### Map genome build synonyms ####
    if(toupper(style)=="NCBI"){
        style <- "Ensembl"
        use_ncbi <- TRUE
    } else {
        use_ncbi <- FALSE
    }
    dat <- GenomeInfoDb::mapGenomeBuilds(genome = genome,
                                         style = style)
    
    if(nrow(dat)==0){
        stopper("Could not recognize genome.")
    }
    #### Use latest version (bottom) #### 
    if(use_ncbi){
        genome2 <- tail(grep("NCBI",dat$ensemblID, value = TRUE),1)
    } else{
        if(toupper(style)=="UCSC"){
            
            genome2 <- utils::tail(dat$ucscID,1)
        } else {
            genome2 <- utils::tail(grep("GRCh",dat$ensemblID, value = TRUE),1)
        } 
    }  
    if(omit_subversion){
        genome2 <- strsplit(genome2,"[.]")[[1]][1]
    }
    if(is.na(genome2) || length(genome2)==0){
        stopper("Could not identify genome translation.")
    }
    return(genome2)
}
