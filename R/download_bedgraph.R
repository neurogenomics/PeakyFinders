download_bedgraph <- function(bedgraph_path,
                              decompress=TRUE){
    if(!is_local(bedgraph_path)){
        bedgraph_path2 <- paste(tempdir(),
                                basename(bedgraph_path),
                                sep = "/")
        utils::download.file(url = bedgraph_path, 
                             destfile = bedgraph_path2) 
        bedgraph_path <- bedgraph_path2
    }
    #### Must be unzipped to be recognized by MACSr ####
    if(isTRUE(decompress)){
        if(R.utils::isGzipped(bedgraph_path)){
            bedgraph_path <- R.utils::gunzip(bedgraph_path, 
                                             overwrite=TRUE)
        } 
    } 
    return(bedgraph_path)
}