#' Get GEO supplementary files.
#' 
#' Get links to any GEO supplementary files matches one or more of the 
#' \code{searches}.
#' \emph{IMPORTANT:} When downloading files from GEO, make sure
#' "ftp://" are replaced with "http://" in URLs, as the former can
#' cause download issues on certain machines. 
#' @param gsm GEO GSM id.
#' Matches will be case-insensitive.
#' @param verbose Print messages.
#' @inheritParams construct_searches
#' 
#' @returns Named list. 
#' 
#' @keywords internal
#' @importFrom methods new
#' @importFrom GEOquery getGEOSuppFiles
get_geo_links <- function(gsm,
                          searches = construct_searches(),
                          verbose=TRUE){ 
    messager("Determining available file types.",v=verbose) 
    #### Get metadata ####
    g <-  GEOquery::getGEOSuppFiles(GEO = gsm, 
                                    makeDirectory = FALSE,
                                    baseDir = tempdir(),
                                    fetch_files = FALSE) 
    supp_urls <- g$url
    # g <- GEOquery::getGEO(GEO = gsm) 
    #### Determine file types ####
    # supp_urls <- g@header[
    #     grep("^supplementary_file_*",names(g@header))
    # ]
    #### Replace ftp with https for more consistent downloads ####
    # supp_urls <- gsub("ftp://","https://",supp_urls) 
    # messager(length(supp_urls),"supplementary file(s) found:",
    #          paste("\n -",paste("...",basename(unlist(supp_urls)),sep="/"),
    #                collapse = ""),
    #          v=verbose)
    links <- mapply(searches,FUN=function(x){
        grep(x, unlist(supp_urls),
             ignore.case = TRUE, value = TRUE)
    }, SIMPLIFY = FALSE) 
    #### Remove empty slots #####
    links <- links[mapply(links, FUN=function(x){length(x)>0})] 
    #### Report ####
    messager("Found file link(s) for",length(links),
             if(length(links)>1) "categories." else "category.") 
    for(nm in names(links)){
        messager(nm,":",
                 paste("\n>>>",links[[nm]], collapse = "")
                 )
    }
    return(links)
}
