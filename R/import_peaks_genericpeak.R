import_peaks_genericpeak <- function(paths,
                                     query_granges=NULL,
                                     nThread=1,
                                     verbose=TRUE){
    messager("Importing pre-computed generic peak files.",v=verbose)
    peaks <- lapply(paths, function(f){
        messager(" -",f,v=verbose)
        #### Try with  rtracklayer ####
        gr <- tryCatch({
            p <- rtracklayer::import(f, which=query_granges)
            p <- add_mcol(gr = p,
                          name = "source", 
                          value =  basename(f))
            return(p)
        }, error = function(e){message(e);NULL})
        if(!is.null(gr)) return(gr)
        #### Try with data.table ####
        gr <- tryCatch({
            ### not all files parse very cleanly 
            dat <- data.table::fread(input = f, 
                                     nThread = nThread)
            col1 <- strsplit(colnames(dat)[1]," ")[[1]]
            if((length(col1)>1) && 
               all(startsWith(colnames(dat)[-1],"V"))){
                colnames(dat) <- tolower(
                    gsub("^peak_","",col1[col1!=""],
                         ignore.case = TRUE)
                )
                colnames(dat) <- gsub("^chrom","chr",colnames(dat))
            }
            if(all(startsWith(colnames(dat),"V"))){
                colnames(dat)[seq_len(3)] <- c("chr","start","end")
            } 
            p <- dt_to_granges(dat = dat, 
                             chrom_col = "chr", 
                             start_col = "start",
                             end_col = "end", 
                             style = "UCSC", 
                             verbose = verbose) 
            p <- add_mcol(gr = p,
                          name = "source", 
                          value =  basename(f)) 
            return(p) 
        }, error = function(e){message(e);GenomicRanges::GRanges()})
    }) |> 
        unlist() |> 
        GenomicRanges::GRangesList() |> 
        unlist()  
    ### Add peaktype ###
    peaks <- add_mcol(gr = peaks,
                      name = "peaktype", 
                      value =  "genericPeak",
                      verbose = verbose) 
    return(peaks)
}
