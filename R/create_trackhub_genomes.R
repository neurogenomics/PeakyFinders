create_trackhub_genomes <- function(genomesFile,
                                    trackdbFile,
                                    genome,
                                    as_list=TRUE,
                                    verbose=TRUE){  
    #### Create genomes.txt ####
    messager("Creating file:",basename(genomesFile),v=verbose)
    trackDb <- top_dir(path = trackdbFile, 
                       n = 2, 
                       invert = TRUE)
    genomes <- list(genome=genome,
                    trackDb=trackDb)
    genomes_char <- list_to_char(lst = genomes)    
    ##### Write ####
    messager("Writing ==>",genomesFile,v=verbose)
    dir.create(dirname(genomesFile),showWarnings = FALSE, recursive = TRUE)
    writeLines(text = genomes_char, 
               con = genomesFile)
    #### Return ####
    if(isTRUE(as_list)){
        return(genomes)
    } else {
        return(genomes_char)
    }
}
