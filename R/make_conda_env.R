#' Make conda environment
#' 
#' Make a dedicated conda environment for \pkg{PeakyFinders}.
#' @inheritDotParams echoconda::yaml_to_env
#' @returns Conda env name.
#'
#' @keywords internal
make_conda_env <- function(...){

    if(!requireNamespace("echoconda", quietly = TRUE)){
        stopper("Package 'echoconda' is required for conda environment setup. ",
                "Install it with: remotes::install_github('RajLabMSSM/echoconda')")
    }
    file <- system.file("conda","PeakyFinders.yml",
                        package = "PeakyFinders")
    conda_env <- echoconda::yaml_to_env(yaml_path = file,
                                        ...)
    return(conda_env)
}