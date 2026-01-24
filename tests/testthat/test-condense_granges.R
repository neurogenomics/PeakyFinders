test_that("condense_granges works", {

    # Create test GRanges with multiple ranges on same chromosome
    gr <- unlist(
        GenomicRanges::GRangesList(
            "gr1" = GenomicRanges::GRanges("chr4:1-100"),
            "gr2" = GenomicRanges::GRanges("chr4:300-500"),
            "gr3" = GenomicRanges::GRanges("chr8:40-10000")
        )
    )

    # Test basic condensing
    gr2 <- condense_granges(gr = gr)
    testthat::expect_true(methods::is(gr2, "GRanges"))
    # Should condense to 2 ranges (one per chromosome)
    testthat::expect_equal(length(gr2), 2)

    # Check chr4 was condensed to span 1-500
    chr4_range <- gr2[GenomicRanges::seqnames(gr2) == "chr4"]
    testthat::expect_equal(GenomicRanges::start(chr4_range), 1)
    testthat::expect_equal(GenomicRanges::end(chr4_range), 500)

    # Check chr8 range preserved
    chr8_range <- gr2[GenomicRanges::seqnames(gr2) == "chr8"]
    testthat::expect_equal(GenomicRanges::start(chr8_range), 40)
    testthat::expect_equal(GenomicRanges::end(chr8_range), 10000)

    # Test split_chromosomes option
    gr3 <- condense_granges(gr = gr, split_chromosomes = TRUE)
    testthat::expect_true(methods::is(gr3, "GRangesList"))
    testthat::expect_equal(length(gr3), 2)
})
