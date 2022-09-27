#' Make conda environment
#' 
#' Make a dedicated conda environment for \pkg{PeakyFinders}.
#' @inheritDotParams echoconda::yaml_to_env
#' @returns Conda env name.
#' 
#' @keywords internal
#' @importFrom echoconda yaml_to_env
make_conda_env <- function(...){
    
    requireNamespace("echoconda")
    file <- system.file("conda","PeakyFinders.yml",
                        package = "PeakyFinders")
    conda_env <- echoconda::yaml_to_env(yaml_path = file,
                                        ...)
    return(conda_env)
}