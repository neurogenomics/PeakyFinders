#' Get chromosomes
#' 
#' Query a bedGraph file by a subset of chromosomes.
#' This allows you to query whole chromosomes at a time, without needing to
#' import the entire bedGraph file.
#' @source \href{https://github.com/Bioconductor/BiocGenerics/issues/12}{
#' GitHub Issues: conflicts with BiocGenerics}
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
#' @keywords internal
#' @importFrom GenomicRanges seqnames
#' @importFrom rtracklayer import export
#' @importFrom BiocGenerics %in%
import_bedgraph_chroms <- function(URL, 
                                   chroms = NULL,
                                   build = "hg38",
                                   import_format = "bedGraph",
                                   export_format = import_format,
                                   save_path = NULL,
                                   verbose = TRUE){
    requireNamespace("regioneR")
    if(tolower(build) %in% c("hg19","grch37")){
        requireNamespace("BSgenome.Hsapiens.UCSC.hg19.masked")     
    }
    if(tolower(build) %in% c("hg38","grch38")){
        requireNamespace("BSgenome.Hsapiens.UCSC.hg38.masked")     
    } 
    messager("Importing chromosome(s):",
             paste(chroms,collapse = ","),v=verbose) 
    select_chrom <- get_genome(genome = build, 
                               keep.chr = chroms,
                               style = "UCSC") 
    if(length(select_chrom)==0) {
        stop("No matching chromosomes could be identified.\n")
    }
    messager("+ Importing as:",import_format,v=verbose)
    if(tolower(import_format)=="bigwig"){
        is_windows <- rtracklayer_bigwig_error()
    }
    gr <- rtracklayer::import(con = URL, 
                              which = select_chrom,
                              format = import_format) 
    # gr <- rtracklayer::import.bw(
    #     con = URL,
    #     which = dt_to_granges(select_chrom, style = 'NCBI'))
    # path <- file.path("~/Downloads",basename(URL))
    # download.file(URL, path)
    # path <- R.utils::gunzip(path, destname = gsub(".bw$","2.bw",path),
    #                         overwrite=TRUE)
    # path
   #### Save or return directly ####
    if(!is.null(save_path)) { 
        messager("+ Writing chromosome subset ==>",save_path, v=verbose)
        rtracklayer::export(object = gr, 
                            con = save_path, 
                            format = export_format)
        return(save_path)
    } else {
        messager("Returning",import_format,"of length",
                 formatC(length(gr),big.mark = ","),v=verbose)
        return(gr)
    } 
}
