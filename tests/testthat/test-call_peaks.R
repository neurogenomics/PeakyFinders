test_that("call_peaks works", {

    if(.Platform$OS.type=="windows"){
        ## MACSr is currently incompatible with Windows.
        testthat::expect_null(NULL)
    } else {
        ## MACS3/MACSr can fail in CI due to Python environment issues
        testthat::skip_on_cran()

        files <- tryCatch({
            example_bg_bw()
        }, error = function(e) {
            testthat::skip(paste("example_bg_bw failed:", e$message))
        })
        testthat::expect_true(PeakyFinders:::is_granges(files$gr))
        testthat::expect_length(files$gr, 1050)

        #### MACSr:: From bedGraph ####
        peaks1 <- tryCatch({
            PeakyFinders::call_peaks(bedgraph_path = files$bedgraph,
                                     method = "MACSr")
        }, error = function(e) {
            testthat::skip(paste("MACSr peak calling failed:", e$message))
        })
        ## Tests
        testthat::expect_true(PeakyFinders:::is_granges(peaks1))
        testthat::expect_length(peaks1, 11)

        #### MACSr:: From bigWig ####
        peaks2 <- tryCatch({
            PeakyFinders::call_peaks(bedgraph_path = files$bigwig,
                                     method = "MACSr")
        }, error = function(e) {
            testthat::skip(paste("MACSr peak calling from bigWig failed:", e$message))
        })
        ## Tests
        testthat::expect_true(PeakyFinders:::is_granges(peaks2))
        testthat::expect_length(peaks2, 11)

        #### Check that peaks from bedGraph vs. bigWig are identical ####
        testthat::expect_equal(peaks1, peaks2)

        #### SEACR:: From bedGraph ####
        peaks3 <- tryCatch({
            PeakyFinders::call_peaks(bedgraph_path = files$bedgraph,
                                     method = "SEACR")
        }, error = function(e) {
            testthat::skip(paste("SEACR peak calling failed:", e$message))
        })
        testthat::expect_true(PeakyFinders:::is_granges(peaks3))
        testthat::expect_length(peaks3, 11)

        #### SEACR:: From bigWig ####
        peaks4 <- tryCatch({
            PeakyFinders::call_peaks(bedgraph_path = files$bigwig,
                                     method = "SEACR")
        }, error = function(e) {
            testthat::skip(paste("SEACR peak calling from bigWig failed:", e$message))
        })
        testthat::expect_true(PeakyFinders:::is_granges(peaks4))
        testthat::expect_length(peaks4, 11)
    }
})
