test_that("call_peaks works", {
    
    if(.Platform$OS.type=="windows"){
        ## MACSr is currently incompatible with Windows.
        testthat::expect_null(NULL)
    } else { 
        files <- example_bg_bw()
        testthat::expect_true(PeakyFinders:::is_granges(files$gr))
        testthat::expect_length(files$gr,1050)
        
        #### MACSr:: From bedGraph ####  
        peaks1 <- PeakyFinders::call_peaks(bedgraph_path = files$bedgraph, 
                                           call_peaks_method = "MACSr")
        ## Tests 
        testthat::expect_true(PeakyFinders:::is_granges(peaks1))
        testthat::expect_length(peaks1, 11)
        
        #### MACSr:: From bigWig ####    
        peaks2 <- PeakyFinders::call_peaks(bedgraph_path = files$bigwig,
                                           call_peaks_method = "MACSr")
        ## Tests
        testthat::expect_true(PeakyFinders:::is_granges(peaks2))
        testthat::expect_length(peaks2, 11)
        
        #### Check that peaks from bedGraph vs. bigWig are identical ####
        testthat::expect_equal(peaks1, peaks2)
        
        #### SEACR:: From bedGraph ####    
        peaks3 <- PeakyFinders::call_peaks(bedgraph_path = files$bedgraph,
                                           call_peaks_method = "SEACR")
        testthat::expect_true(PeakyFinders:::is_granges(peaks3))
        testthat::expect_length(peaks3, 11)
        
        #### SEACR:: From bigWig ####   
        peaks4 <- PeakyFinders::call_peaks(bedgraph_path = files$bigwig,
                                           call_peaks_method = "SEACR")
        testthat::expect_true(PeakyFinders:::is_granges(peaks4))
        testthat::expect_length(peaks4, 11)
    }
})
