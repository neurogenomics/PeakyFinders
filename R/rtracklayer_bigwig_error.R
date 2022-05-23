rtracklayer_bigwig_error <- function(){
    is_windows <- .Platform$OS.type=="windows"
    if(is_windows){
        stop(
            "rtracklayer::import.bw() does not work on Windows.",
            "Please contact the rtracklayer developers to address this issue.")
    }
    return(is_windows)
}
