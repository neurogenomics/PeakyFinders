test_that("pooled_peaks works", { 
    
    bam_files <- example_bam() 
    #### MACSr ####
    peaks1 <- pooled_peaks(bam_files = bam_files,
                           method = "MACSr") 
    
    #### SEACR ####
    peaks2 <- pooled_peaks(bam_files = bam_files,
                           method = "SEACR") 
    testthat::expect_true(PeakyFinders:::is_granges(peaks2))
    
    #### SEACR: with groups ####
    groups <- c("group1","group2")
    peaks3 <- pooled_peaks(bam_files = bam_files,
                           groups = groups,
                           method = "SEACR",
                           save_dir = file.path(tempdir(),"grouped")) 
    testthat::expect_true(is.list(peaks3))
    testthat::expect_equal(names(peaks3),groups)
    testthat::expect_true(all(
        unlist(lapply(peaks3, PeakyFinders:::is_granges))
    ))
    testthat::expect_length(peaks3[[1]], 12656)
})
