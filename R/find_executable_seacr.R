#' Find executable: \pkg{SEACR}
#' 
#' Find path to the executable file for \pkg{SEACR}.
#' @inheritParams echoconda::find_packages
#' @inheritDotParams echoconda::find_packages
#' @keywords internal
#' @returns Path to \pkg{SEACR} executable.
#' @source \href{https://github.com/FredHutch/SEACR/issues/54}{
#' GitHub Issue: illegal byte sequence}
#' 
#' @importFrom echoconda find_packages
find_executable_seacr <- function(packages=c("SEACR_1.3.sh",
                                             "SEACR_1.3.R"),
                                  fix_script=TRUE,
                                  verbose=TRUE,
                                  ...){
    
    conda_env <- make_conda_env(verbose=verbose)
    pkgs <- echoconda::find_packages(packages = packages,
                                     conda_env = conda_env,
                                     verbose = verbose,
                                     return_path = TRUE,
                                     ...)
    #### Fix bugs in the SEACR bash script ####
    sh <- pkgs[endsWith(names(pkgs),".sh")]
    if(isTRUE(fix_script) && 
       length(sh)>0){
        messager("Fixing script:",basename(sh),v=verbose)
        l <- readLines(sh)
        i <- grep("^password",l)
        if(length(i)>0){
            l[i] <- c("password=${RANDOM}",
                      "password2=${RANDOM}")
            writeLines(l,con = sh)
        } 
    } 
    #### Fix bugs in the SEACR R script ####
    r <- pkgs[endsWith(names(pkgs),".R")]
    if(isTRUE(fix_script) && 
       length(r)>0){
        messager("Fixing script:",basename(r),v=verbose)
        l <- readLines(r)
        extra_line <- "library(utils);library(stats);"
        if(all(isFALSE(grepl(extra_line,l)))){
            l <- c("library(utils);library(stats);",l) 
            writeLines(l,con = r)
        } 
    } 
    return(pkgs)
}