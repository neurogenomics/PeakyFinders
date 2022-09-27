seacr_tmp_files <- function(outdir){
    tmp_files <- list.files(outdir, 
                            pattern = paste(
                                c("\\.fdr\\.txt$",
                                  "\\.auc\\.bed$",
                                  "\\.auc$",
                                  "\\.threshold\\.bed$",
                                  "\\.threshold\\.txt$"),
                                collapse = "|"
                            ), full.names = TRUE) 
    return(tmp_files)
}
