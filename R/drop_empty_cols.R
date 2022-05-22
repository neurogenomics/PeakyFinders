drop_empty_cols <- function(dt,
                            verbose=TRUE){
    for(col in names(dt)){
        if(all(is.na(dt[[col]]))){
            messager("Removing:",col,v=verbose)
            dt[[col]] <- NULL
        }
    }
}