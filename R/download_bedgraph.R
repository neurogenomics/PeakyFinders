download_bedgraph <- function(bedgraph_path){
    if(!is_local(bedgraph_path)){
        bedgraph_path2 <- paste(tempdir(),
                                basename(bedgraph_path),
                                sep = "/")
        utils::download.file(url = bedgraph_path, 
                             destfile = bedgraph_path2)
        if(R.utils::isGzipped(bedgraph_path2)){
            bedgraph_path2 <- R.utils::gunzip(bedgraph_path2, 
                                              overwrite=TRUE)
        } 
        bedgraph_path <- bedgraph_path2
    }
    return(bedgraph_path)
}