top_dir <- function(path,
                    n=1,
                    invert=FALSE,
                    concat=TRUE){
    
    split <- grep("^$",strsplit(x = path, 
                                split = .Platform$file.sep)[[1]],
                  invert = TRUE, 
                  value = TRUE)
    if(isFALSE(invert)){
        split <- split[seq_len(n)]
    } else {
        split <- split[seq((length(split)-n+1),length(split))]
    }
    if(isTRUE(concat)){
        return(Reduce(file.path,split))
    } else {
        return(split)
    }
}