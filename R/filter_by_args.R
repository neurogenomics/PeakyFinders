filter_by_args <- function(meta,
                           arg_list,
                           partial_match=TRUE,
                           verbose=TRUE){ 
    # arg_list <- as.list(match.call(definition = search_roadmap))[-1]
    arg_list <- arg_list[names(arg_list) %in% colnames(meta)]
    if(length(arg_list)==0) {
        # messager("Returning all metadata:",
        #          formatC(nrow(meta),big.mark = ","),
        #          "entries.",v=verbose)
        return(meta)
    }
    #### Filter data #### 
    for(arg in names(arg_list)){ 
        if(arg %in% colnames(meta)){ 
            messager("Filtering by:",arg,v=verbose)
            if(partial_match){
                meta <- meta[
                    grepl(paste(arg_list[[arg]], collapse = "|"), 
                          meta[[arg]], ignore.case = TRUE),
                ] 
            } else {
                meta <- subset(meta, 
                               eval(parse(text=arg))==arg_list[[arg]]) 
            }
            
        } 
    }
    return(meta)
}
