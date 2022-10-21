is_macs <- function(gr,
                    path){ 
    
    #### Remove cols that are entirely "." ####
    gr <- gr[,!unlist(lapply(GenomicRanges::mcols(gr), 
                             function(x){all(x==".")}))]
    ncols <- ncol(GenomicRanges::mcols(gr))
    grepl("macs|narrowpeak|broadpeak",path,ignore.case = TRUE) &&
        ncols >=6 && 
    (
        all(
            unlist(lapply(GenomicRanges::mcols(gr), typeof)[seq_len(6)])==
                c("character",rep("double",4),"integer")
        )  |
        all(
            unlist(lapply(GenomicRanges::mcols(gr), typeof)[seq_len(6)])==
                c("character","integer",rep("double",3),"integer")
        )  |
            all(
                unlist(lapply(GenomicRanges::mcols(gr), typeof)[seq_len(5)])==
                    c("character","integer",rep("double",3))
            )  
    )
}