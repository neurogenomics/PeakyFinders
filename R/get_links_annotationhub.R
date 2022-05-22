get_links_annotationhub <- function(meta, 
                                    ids,
                                    searches,
                                    build,
                                    split_by_id=TRUE,
                                    verbose=TRUE){ 
    
    ah_id <- biosample_id <- title <- NULL;
    
    ids <- stats::setNames(ids,ids)
    links_list <- mapply(ids,
                         SIMPLIFY = FALSE,
                         FUN=function(eid){
         messager("Searching AnnotationHub metadata for:",
                  eid,v=verbose) 
         #### Filter metadata further using IDs ####
         meta2 <- meta[(ah_id == eid)|
                       (title == eid),] 
         #### Get links for each id ####
         if(nrow(meta2)==0) {
             messager(
                 "WARNING:",
                 "No matching AnnotationHub file retrieved.",
                 "Returning NULL."
             )
             links <- NULL
         } else {
             ## genericPeak is unnecessary here and 
             ## imports same peaks twice
             peak_types <- c("narrowpeak","broadpeak","gappedpeak")
             if(any(peak_types %in% names(searches))){
                 searches$genericpeak <- NULL 
             }  
             ## Iterate over search keys
             links <- lapply(names(searches), function(nm){
                 m <- meta2[
                     grepl(searches[[nm]], x = meta2$title, 
                           ignore.case = TRUE)|   
                     grepl(searches[[nm]], x = meta2$description, 
                           ignore.case = TRUE)| 
                     grepl(searches[[nm]], x = meta2$sourcetype,
                           ignore.case = TRUE)|
                     # grepl(searches[[nm]], x = meta2$file_format_type, 
                     #       ignore.case = TRUE)|
                     # grepl(searches[[nm]], x = meta2$output_category,
                     #       ignore.case = TRUE)|
                     # grepl(searches[[nm]], x = meta2$output_type,
                     #       ignore.case = TRUE)|
                     grepl(searches[[nm]], x = meta2$sourceurl, 
                           ignore.case = TRUE),,drop=FALSE
                 ] 
                 if(nrow(m)==0) {
                     return(NULL)
                 } else{ 
                     messager("Identified", 
                              formatC(nrow(m),big.mark = ","),"files:",nm,
                              v=verbose)
                     return(stats::setNames(m$sourceurl,m$ah_id))
                 } 
             }) |> `names<-`(tolower(names(searches))) 
         } 
         return(links)
     })
    return(links_list)
}
