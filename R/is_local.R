#' Is local
#' 
#' Determine whether a file exist locally.
#' @param path Path to local file or remote URL.
#' @keywords internal 
is_local <- function(path) {
    # ssh.utils::file.exists.remote(file = path) # Doesn't work?
    (!is_url(path)) && (file.exists(path))
}
