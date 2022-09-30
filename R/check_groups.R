check_groups <- function(files,
                         groups){
    if(is.null(groups)){
        groups <- rep("all",length(files))
    } else {
        if(length(groups)!=length(files)){
            stopper("groups must be the same length as files or NULL.")
        }
    }
    return(stats::setNames(groups,
                           groups))
}