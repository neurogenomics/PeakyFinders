filter_genome <- function(meta,
                          build,
                          build_col = "genome",
                          verbose = TRUE){ 
    meta <- meta[get(build_col)==build,]
    messager(formatC(nrow(meta),big.mark = ","),
             "rows remain after filtering by genome build:",build,v=verbose)
    return(meta)
}