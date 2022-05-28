bigwig_to_bedgraph <- function(path,
                               verbose=TRUE){
    if(is_bigwig(path)){ 
        messager("Converting bigWig --> bedGraph.",v=verbose)
        gr <- rtracklayer::import(path)
        path2 <- gsub("bigwig$|bw$","bedGraph",path,
                      ignore.case = TRUE)
        rtracklayer::export.bedGraph(object = gr, 
                                     con = path2)
        path <- path2
    } else if(is_bedgraph(path)){
        messager("File already in bedGraph format.",v=verbose)
    } else {
        stopper("File must be in bedGraph or bigWig format.")
    }
    return(path)
}