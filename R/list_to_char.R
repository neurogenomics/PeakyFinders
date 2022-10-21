list_to_char <- function(lst,
                         sep="\t",
                         newline="\n"){ 
    paste(names(lst), lst, 
          sep = sep, 
          collapse = newline)   
}
