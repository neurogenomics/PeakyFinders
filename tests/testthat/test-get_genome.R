test_that("get_genome works", {

    # Test basic usage
    gr <- get_genome(keep.chr = 1:2)
    testthat::expect_true(methods::is(gr, "GRanges"))
    testthat::expect_equal(length(gr), 2)
    testthat::expect_true(all(c("chr1", "chr2") %in% as.character(GenomicRanges::seqnames(gr))))

    # Test with character chromosome names
    gr2 <- get_genome(keep.chr = c("chr1", "chr22"))
    testthat::expect_equal(length(gr2), 2)

    # Test hg38 genome
    gr3 <- get_genome(keep.chr = "chr1", genome = "hg38")
    testthat::expect_true(methods::is(gr3, "GRanges"))
    testthat::expect_equal(length(gr3), 1)

    # Test split_chromosomes
    gr4 <- get_genome(keep.chr = 1:3, split_chromosomes = TRUE)
    testthat::expect_true(is.list(gr4))
    testthat::expect_equal(length(gr4), 3)

    # Test Ensembl style
    gr5 <- get_genome(keep.chr = 1:2, style = "Ensembl")
    testthat::expect_true(methods::is(gr5, "GRanges"))
    testthat::expect_true(all(c("1", "2") %in% as.character(GenomicRanges::seqnames(gr5))))
})
