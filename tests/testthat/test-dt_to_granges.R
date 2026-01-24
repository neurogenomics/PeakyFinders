test_that("dt_to_granges works", {

    # Create test data frame
    dat <- data.frame(
        seqnames = c("chr1", "chr2"),
        start = c(100, 500),
        end = c(200, 600)
    )

    # Test basic conversion
    gr <- dt_to_granges(dat = dat, verbose = FALSE)
    testthat::expect_true(methods::is(gr, "GRanges"))
    testthat::expect_equal(length(gr), 2)

    # Test with custom column names
    dat2 <- data.frame(
        chrom = c("chr1", "chr2"),
        pos_start = c(100, 500),
        pos_end = c(200, 600)
    )
    gr2 <- dt_to_granges(
        dat = dat2,
        chrom_col = "chrom",
        start_col = "pos_start",
        end_col = "pos_end",
        verbose = FALSE
    )
    testthat::expect_true(methods::is(gr2, "GRanges"))
    testthat::expect_equal(length(gr2), 2)

    # Test passing GRanges returns same GRanges
    gr_input <- GenomicRanges::GRanges("chr1:100-200")
    gr3 <- dt_to_granges(dat = gr_input, verbose = FALSE)
    testthat::expect_true(methods::is(gr3, "GRanges"))
    testthat::expect_equal(length(gr3), 1)

    # Test extra columns are preserved
    dat3 <- data.frame(
        seqnames = "chr1",
        start = 100,
        end = 200,
        score = 0.95,
        name = "peak1"
    )
    gr4 <- dt_to_granges(dat = dat3, verbose = FALSE)
    testthat::expect_true("score" %in% names(GenomicRanges::mcols(gr4)))
    testthat::expect_true("name" %in% names(GenomicRanges::mcols(gr4)))
})
