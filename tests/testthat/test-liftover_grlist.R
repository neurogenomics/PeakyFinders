test_that("liftover_grlist works", {
  
    grlist <- list("gr1"=GenomicRanges::GRanges("4:1-100000"),
                   "gr2"=GenomicRanges::GRanges("6:1-100000"),
                   "gr3"=GenomicRanges::GRanges("8:1-100000"))

    grlist_lifted <- liftover_grlist(grlist = grlist,
                                     input_build = "hg19",
                                     output_build="hg38")
    testthat::expect_equal(length(grlist_lifted),
                           length(grlist))
    
    gr_lifted <- liftover_grlist(grlist = grlist[[1]],
                                     input_build = "hg19",
                                     output_build="hg38")
    testthat::expect_true(peakyfinders:::is_granges(gr_lifted))
})
