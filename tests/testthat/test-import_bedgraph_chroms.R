test_that("import_bedgraph_chroms works", {
    
    #### From bedgraph ####
    gsm <- "GSM4703766"
    chroms <- "chr22"
    #### Get links to supplementary files on GEO ####
    links <- PeakyFinders:::get_geo_links(gsm = gsm) 
    #### Import bedgraph subset: chr6 ####
    gr <- PeakyFinders:::import_bedgraph_chroms(URL = links$bedgraph, 
                                                chroms = chroms)
    testthat::expect_true(methods::is(gr,"GRanges"))
    testthat::expect_length(gr, 19004)
    testthat::expect_equal(as.character(unique(GenomicRanges::seqnames(gr))),
                           chroms)
    
    bigwig_test <- function(){
        #### From bigwig ####
        gsm <- "GSM5684359"
        #### Get links to supplementary files on GEO ####
        links <- PeakyFinders:::get_geo_links(gsm = gsm)
        #### Import bigwig subset: chr6 ####
        gr <- PeakyFinders:::import_bedgraph_chroms(URL = links$bigwig,
                                                    chroms = chroms,
                                                    import_format = "BigWig")
        testthat::expect_true(methods::is(gr,"GRanges"))
        testthat::expect_length(gr, 508185)
        testthat::expect_equal(as.character(unique(GenomicRanges::seqnames(gr))),
                               chroms)
    } 
    #### rtracklayer is having some recurring struggles.... ####
    ## https://github.com/lawremi/rtracklayer/issues/63
    if(.Platform$OS.type=="windows"){
        testthat::expect_failure(bigwig_test)
    } else {
        bigwig_test()
    } 
})
