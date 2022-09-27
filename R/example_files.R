example_files <- function(gsm="GSM4703766",
                          ranges = "chr6:165169213-167169213"){
    
    links <- get_geo_links(gsm = gsm) 
    query_granges <- GenomicRanges::GRanges(ranges)
    gr <- rtracklayer::import(con = links$bedgraph,
                              which = query_granges)
    gr <- fix_seqinfo(gr = gr, build = "hg19")
    #### save ####
    bg <- tempfile(fileext = ".bedgraph")
    rtracklayer::export.bedGraph(object = gr, con = bg)
    bw <- tempfile(fileext = ".bigwig") 
    
    rtracklayer::export.bw(object = gr, con = bw)
    return(list(gr=gr,
                bedgraph=bg,
                bigwig=bw))
}