get_links_encode <- function(meta,
                             ids,
                             searches,
                             split_by_id=TRUE,
                             verbose=TRUE){ 
    
    accession <- file_accession <- title <- alternate_accessions <- NULL;
   
    ids <- stats::setNames(ids,ids) 
    links_list <- mapply(ids,
                         SIMPLIFY = FALSE,
                         FUN=function(eid){
         messager("Searching ENCODE metadata for:",
                  eid,v=verbose) 
         #### Filter metadata further using IDs ####
         meta2 <- meta[(accession == eid)|
                       (file_accession == eid)|
                       (title == eid)|
                       (alternate_accessions == eid),]  
         #### Get links for each id ####
         if(nrow(meta2)==0) {
             messager(
                 "WARNING:",
                 "No matching ENCODE file retrieved.",
                 "Returning NULL."
             )
             links <- NULL
         } else {
             ## genericPeak is unnecessary here and 
             ## imports same peaks twice
             peak_types <- c("narrowpeak","broadpeak","gappedpeak","bigbed")
             if(any(peak_types %in% names(searches))){
                 searches$genericpeak <- NULL 
             } 
             ## Iterate over search keys
             links <- lapply(names(searches), 
                             function(nm){
                 m <- meta2[
                     grepl(searches[[nm]], x = meta2$href, 
                           ignore.case = TRUE) |   
                         grepl(searches[[nm]], x = meta2$file_type, 
                               ignore.case = TRUE) |
                         grepl(searches[[nm]], x = meta2$file_format,
                               ignore.case = TRUE),,drop=FALSE
                 ]
                 if(nrow(m)==0) {
                     return(NULL)
                 } else{ 
                     messager("Identified", 
                              formatC(nrow(m),big.mark = ","),"files:",nm,
                              v=verbose)
                     return(stats::setNames(
                         paste0("https://www.encodeproject.org",m$href),
                         m$assembly
                     ))
                 } 
             }) |> `names<-`(tolower(names(searches))) 
         } 
         #### Remove duplicated bigbed files from other categories ### 
         if(!is.null(links$bigbed)){
             messager("Removing duplicated bigBed files.",v=verbose)
             for(l in links$bigbed){
                 if(l %in% unlist(links[names(links)!="bigbed"])){
                     links$bigbed <- links$bigbed[links$bigbed!=l]
                 }
             } 
             links[lapply(links, length)==0] <- NULL
         }
         return(links)
     }) 
    return(links_list)
}
