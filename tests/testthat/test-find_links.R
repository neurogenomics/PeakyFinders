test_that("find_links works", {

    ## Skip on CRAN and CI since external URLs can be unreliable
    testthat::skip_on_cran()
    testthat::skip_if_offline()

    #### CATlas ####
    links <- tryCatch({
        find_links(
            urls="http://renlab.sdsc.edu/kai/Key_Processed_Data/ABC_scores/",
            pattern="\\.tsv\\.gz")
    }, error = function(e) {
        testthat::skip(paste("CATlas server unavailable:", e$message))
    })
    testthat::expect_true(nrow(links) > 0)

    #### ENCODE ####
    urls <- paste0("https://www.encodeproject.org/files/",
                  c("ENCFF215OHE","ENCFF163SYA","ENCFF644JTM"))
    links <- tryCatch({
        find_links(urls = urls,
                   pattern = "(@@download)*.&*.(\\.fastq\\.gz)",
                   as_datatable = FALSE)
    }, error = function(e) {
        testthat::skip(paste("ENCODE server unavailable:", e$message))
    })
    testthat::expect_equal(names(links),basename(urls))
    testthat::expect_true(length(unlist(links)) > 0)
    testthat::expect_true(all(endsWith(unlist(links),".fastq.gz")))
})
