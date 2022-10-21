create_trackhub_description <- function(file,
                                        trackdbFile,
                                        verbose=TRUE){
    
    descriptionUrl <-  file.path(dirname(trackdbFile),
                                 paste0(basename(file),".html"))
    messager("Creating file:",basename(descriptionUrl),v=verbose) 
    
    writeLines(text = basename(file), 
               con = descriptionUrl)
    return(descriptionUrl)
}
