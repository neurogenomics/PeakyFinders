create_trackhub_hub <- function(hub,
                                descriptionUrl,
                                shortLabel,
                                longLabel,
                                email,
                                hubFile,
                                genomesFile,
                                as_list=TRUE,
                                verbose=TRUE){  
    #### Create genomes.txt ####
    messager("Creating file:",basename(hubFile),v=verbose)
    hub_lst <- list(hub=hub,
                    descriptionUrl=descriptionUrl,
                    shortLabel=shortLabel,
                    longLabel=longLabel,
                    genomesFile=basename(genomesFile),
                    email=email)   
    hub_char <- list_to_char(lst = hub_lst)    
    ##### Write ####
    messager("Writing ==>",hubFile,v=verbose)
    dir.create(dirname(hubFile),showWarnings = FALSE, recursive = TRUE)
    writeLines(text = hub_char, 
               con = hubFile)
    #### Return ####
    if(isTRUE(as_list)){
        return(hub_lst)
    } else {
        return(hub_char)
    }
}
