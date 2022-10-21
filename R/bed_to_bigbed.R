bed_to_bigbed <- function(files,
                          save_dir=file.path(tempdir(),"bigbed")){
    lapply(files,
           function(y){
               message("Converting: ",basename(y))                                        
               y2 <- file.path(save_dir,
                               gsub("\\.bed$",".bigBed",basename(y)))
               dir.create(dirname(y2), 
                          showWarnings = FALSE, recursive = TRUE)
               # gr <- rtracklayer::import(y, format="narrowPeak")
               # gr <- ChIPseeker::readPeakFile(peakfile = y) 
               gr <- import_peaks(ids = y)[[1]][[1]]
               if(!"score" %in% names(GenomicRanges::mcols(gr))){
                   GenomicRanges::mcols(gr)$score <- gr$total_signal
               }
               gr <- fix_seqinfo(gr = gr, build = "hg19")
               rtracklayer::export.bb(gr, con = y2)
               # gr2 <- rtracklayer::import(con = y2)
               y <- y2 
               return(y)
       }) 
}