test_that("call_peaks works", {
    
    
    gsm <- "GSM4703766" 
    links <- PeakyFinders:::get_geo_links(gsm = gsm) 
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
    gr <- rtracklayer::import(con = links$bedgraph,
                              which = query_granges)
    testthat::expect_true(PeakyFinders:::is_granges(gr))
    testthat::expect_length(gr,1050)
    
    #### From bedGraph #### 
    tmp <- tempfile(fileext = ".bedgraph")
    rtracklayer::export.bedGraph(object = gr, con = tmp)
    peaks1 <- PeakyFinders::call_peaks(bedgraph_path = tmp)
    ## Tests 
    testthat::expect_true(PeakyFinders:::is_granges(peaks1))
    testthat::expect_length(peaks1, 11)
    
    #### From bigWig #### 
    tmp <- tempfile(fileext = ".bigwig") 
    gr <- PeakyFinders:::fix_seqinfo(gr = gr, build = "hg19")
    rtracklayer::export.bw(object = gr, con = tmp)
    peaks2 <- PeakyFinders::call_peaks(bedgraph_path = tmp)
    ## Tests
    testthat::expect_true(PeakyFinders:::is_granges(peaks2))
    testthat::expect_length(peaks2, 11)
    
    #### Check that peaks from bedGraph vs. bigWig are identical ####
    testthat::expect_equal(peaks1, peaks2)
})
