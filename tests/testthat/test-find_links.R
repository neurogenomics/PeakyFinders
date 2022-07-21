test_that("find_links works", {
  
    #### CATlas ####
    links <- find_links(
        urls="http://renlab.sdsc.edu/kai/Key_Processed_Data/ABC_scores/",
        pattern="\\.tsv\\.gz")
    testthat::expect_equal(nrow(links),111)
    
    #### ENCODE ####
    urls <- paste0("https://www.encodeproject.org/files/",
                  c("ENCFF215OHE","ENCFF163SYA","ENCFF644JTM"))
    links <- find_links(urls = urls, 
                        pattern = "(@@download)*.&*.(\\.fastq\\.gz)",
                        as_datatable = FALSE)
    testthat::expect_equal(names(links),basename(urls))
    testthat::expect_length(unlist(links),6)
    testthat::expect_true(all(endsWith(unlist(links),".fastq.gz")))
})
