parse_build <- function(builds,
                        id,
                        verbose=TRUE){
    build <- if(length(builds)==1) {
        builds
    } else if (id %in% names(builds)){
        builds[[id]]
    }
    messager("Using build:",build,v=verbose)
    return(build)
}
