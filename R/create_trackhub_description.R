create_trackhub_description <- function(file,
                                        path,
                                        domain,
                                        trackdbFile,
                                        verbose=TRUE){
    
    descriptionUrl <-  file.path(dirname(trackdbFile),
                                 paste0(basename(file),".html"))
    messager("Creating file:",descriptionUrl,v=verbose) 
    descriptionUrl_remote <- gsub(path,domain,descriptionUrl)
    dir.create(dirname(descriptionUrl), showWarnings = FALSE, recursive = TRUE)
    writeLines(text = descriptionUrl_remote, 
               con = descriptionUrl)
    return(descriptionUrl_remote)
}
