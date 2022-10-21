categorise_links <- function(links_list,
                             searches,
                             verbose=TRUE){
    links <- mapply(searches,
                    FUN=function(x){
        grep(x, unlist(links_list),
             ignore.case = TRUE, value = TRUE)
    }, SIMPLIFY = FALSE) 
    #### Remove empty slots #####
    links <- links[mapply(links, FUN=function(x){length(x)>0})] 
    #### Report ####
    messager("Found file link(s) for",length(links),
             if(length(links)>1) "categories." else "category.",v=verbose) 
    for(nm in names(links)){
        messager(nm,":",
                 paste("\n>>>",links[[nm]], collapse = "")
        )
    }
    return(links)
}
