is_bedgraph <- function(path){
    grepl("bedgraph$|bg$",path,ignore.case = TRUE)
}