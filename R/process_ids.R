process_ids <- function(ids, 
                        verbose=TRUE){
    # ids=c("GSE188512","GSM5684359") 
    utils::data("peaks_metadata_roadmap", package = "PeakyFinders")
    peaks_metadata_roadmap <- get("peaks_metadata_roadmap")
    messager("Processing id(s).",v=verbose)
    id_matches <- function(ids,
                           db="GEO"){ 
        rm_ids <- unique(peaks_metadata_roadmap$ah_id)
        db_prefixes <- list("GEO"=c("GSE","GSM"),
                            "ENCODE"=c("ENCSR","c818e","ENCFF"),
                            "ROADMAP"=c("E0"),
                            "AnnotationHub"=c("AH"))  
        #### Check for files not matching any other db type ####
        if(db=="file"){
            ids2 <- grep(pattern = paste0("^",unlist(db_prefixes),
                                          collapse = "|"),
                         x = ids,
                         invert = TRUE,
                         ignore.case = TRUE, 
                         value = TRUE)
            return(ids2)
        }
        ids2 <- grep(pattern = paste0("^",db_prefixes[[db]],collapse = "|"),
                     x = ids,
                     ignore.case = TRUE, 
                     value = TRUE)
        ids2 <- if(db=="ROADMAP") {
            #### Add ROADMAP AnnotationHub ids ####
            c(ids2, ids[ids %in% rm_ids])
        } else if(db=="AnnotationHub"){
            #### Remove ROADMAP AnnotationHub ids ####
            ## To avoid double-importing same files
            ids2[!ids2 %in% rm_ids]
        } else {ids2} 
        return(unique(ids2))
    } 
    #### GEO #### 
    geo_ids <- id_matches(ids = ids,
                          db = "GEO")
    geo_ids <- lapply(ids, function(id){ 
        ## Get GSM samples from GSE id.
        if(grepl("^GSE", id, ignore.case = TRUE)){
            g <- GEOquery::getGEO(GEO = id) 
            samples <- unlist(
                lapply(g, function(x){
                    x[["geo_accession"]]
                }) 
            ) 
            samples <- c(stats::setNames(id,id),samples)
        } else if(grepl("^GSM", id, ignore.case = TRUE)){
            samples <- id
        } else{ 
            samples <- NULL
        }
        return(samples)
    })
    geo_ids <- unlist(geo_ids)[!duplicated(unlist(geo_ids))] 
    if(length(geo_ids)>0){
        messager(length(geo_ids),"unique GEO id(s) identified.",v=verbose)
    } else {
        geo_ids <- character()
    }
    #### ENCODE ####
    # meta <- search_encode() 
    # unique(stringr::str_sub(meta$accession,1,5))
    # unique(stringr::str_sub(meta$file_accession,1,5))
    # unique(stringr::str_sub(meta$alternate_accessions,1,5)) 
    encode_ids <- id_matches(ids = ids,
                             db = "ENCODE") 
    if(length(encode_ids)>0){
        messager(length(encode_ids),"unique ENCODE id(s) identified.",
                 v=verbose)
    }
    #### ROADMAP ####
    roadmap_ids <- id_matches(ids = ids,
                              db = "ROADMAP") 
    if(length(roadmap_ids)>0){
        messager(length(roadmap_ids),"unique ROADMAP id(s) identified.",
                 v=verbose)
    }
    #### AnnotationHub ####
    ah_ids <- id_matches(ids = ids,
                         db = "AnnotationHub") 
    if(length(ah_ids)>0){
        messager(length(ah_ids),"unique AnnotationHub id(s) identified.",
                 v=verbose)
    }
    ##### Local/Remote files ####
    file_ids <- id_matches(ids = ids,
                           db = "file")
    if(length(file_ids)>0){
        messager(length(file_ids),"unique local file paths(s) identified.",
                 v=verbose)
    }
    #### Return named list ####
    return(list(
        GEO=geo_ids,
        ENCODE=encode_ids,
        ROADMAP=roadmap_ids,
        AnnotationHub=ah_ids,
        file=file_ids
    ))
}
