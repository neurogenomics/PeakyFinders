#' Find links
#' 
#' Scrape a web page for links.
#' @param urls List of URLs to search for links on.
#' @param pattern Regex pattern to search for files with.
#' @param as_datatable Return the results as a \link[data.table]{data.table}
#'  (default: \code{TRUE}), or a nested list (\code{FALSE}).
#' @param workers Number of threads of parallelize across.
#' @returns A data.table or named list.
#' 
#' @importFrom parallel mclapply
#' @export
#' @examples
#' links <- find_links(
#'     urls="http://renlab.sdsc.edu/kai/Key_Processed_Data/ABC_scores/",
#'     pattern="\\.tsv\\.gz")
find_links <- function(urls,
                       pattern=NULL,
                       as_datatable=TRUE,
                       workers=1){
    
    requireNamespace("rvest")
    #### Name each url ####
    if(length(names(urls))!=length(urls)){
        message("Adding names to urls using basename.")
        names(urls) <- make.unique(basename(urls))
    } 
    urls <- as.list(urls) 
    LINKS <- parallel::mclapply(urls, 
                                pattern = pattern,
                                FUN=function(URL,pattern){
        tryCatch({
            page <- rvest::read_html(URL)
            links <- paste0(URL,
                            page |>
                                rvest::html_nodes("a")|>
                                rvest::html_attr("href") ) |>
                grep(pattern = "\\.\\.$", invert = TRUE, value = TRUE)
            #### Remove links to next directory up ####
            suppressWarnings(nms <- basename(links))
            if(!is.null(pattern)){
                links <- grep(pattern = pattern, links, value = TRUE)
                nms <- basename(links)
                nms <- gsub(pattern = pattern,"",nms)
            }
            # message("Found ",length(links)," matching links.")
            nms <- gsub("%20"," ",nms) 
            names(links) <- nms
            return(as.list(links))
        }, error=function(e){message(e);NULL})
    })
    message("Found ",length(unlist(LINKS))," matching links.")
    #### Return ####
    if(isTRUE(as_datatable)){
        message("Returning results as data.table.")
        LINKS_df <- mapply(LINKS, 
               SIMPLIFY = FALSE,
               FUN=function(x){
           data.table::data.table(data.frame(link=unlist(x),name=names(x)))
        }) |> data.table::rbindlist(use.names = TRUE, idcol = "query")
        return(LINKS_df) 
    } else {
        message("Returning results as a nested list.")
        return(LINKS)
    } 
}
