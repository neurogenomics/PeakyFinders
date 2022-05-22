#' Unnest data.table wider
#' 
#' A far more efficient version of\link[tidyr]{unnest_wider} for 
#' \link[data.table]{data.table}.
#' 
#' @source \href{https://stackoverflow.com/a/51448422/13214824}{
#' StackOverflow}
#' 
#' @returns \link[data.table]{data.table}. 
#' 
#' @keywords internal
#' @importFrom  data.table setnames dcast :=
unnest_wider_dt <- function(dt, 
                            col, 
                            id_col,
                            new_cols=NULL,
                            verbose=TRUE){ 
    # dt2 <-tidyr::unnest_wider(data = dt[1:10,], 
    #                           col= cl)
    id_col_tmp <- list_col_tmp <- . <- NULL;
    data.table::setnames(dt,id_col,"id_col_tmp") 
    messager("Spreading listed column:",col,v=verbose) 
    #### Make new column names ####
    if(is.null(new_cols)){
        max_len <- max(unlist(lapply(dt[[col]],length)), na.rm = TRUE)
        new_cols <- paste0(col,seq_len(max_len)) 
    }
    data.table::setnames(dt,col,"list_col_tmp")
    dt2 <- data.table::copy(dt)[,list_col_tmp:=NULL]
    for(i in seq_len(length(new_cols))){
        dt2 <- dt2[
            dt[, .(new_col = unlist(.(unlist(list_col_tmp)[i]))  ),
               by = id_col_tmp],
            on = .(id_col_tmp)
        ] |> data.table::setnames("new_col",new_cols[i]) 
    }   
    #### Return id_col to original name ####
    data.table::setnames(dt2,"id_col_tmp",id_col) 
    ##### Get unique values per new column ####
    # unique_items <- mapply(stats::setNames(new_cols,new_cols),
    #                        FUN=function(x){unlist(unique(dt2[[x]]))})
    return(dt2)
}
