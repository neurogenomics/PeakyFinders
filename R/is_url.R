#' Check if the input is url e.g. http:// or ftp://
#' @param fileName character vector
#' @keywords internal
#' @source \href{https://rdrr.io/cran/seqminer/src/R/seqminer.R}{
#' Borrowed from \code{seqminer} internal function}
is_url <- function(fileName) {
    if (all(grepl(pattern = "^http://", fileName) |
            grepl(pattern = "^ftp://", fileName) )) {
        return(TRUE)
    }
    return(FALSE)
}
