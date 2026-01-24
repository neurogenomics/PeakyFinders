test_that("construct_searches works", {

    # Test default searches
    searches <- construct_searches()
    testthat::expect_true(is.list(searches))
    testthat::expect_true("narrowpeak" %in% names(searches))
    testthat::expect_true("broadpeak" %in% names(searches))
    testthat::expect_true("bedgraph" %in% names(searches))
    testthat::expect_true("bigwig" %in% names(searches))

    # Test filtering by keys
    searches2 <- construct_searches(keys = "narrow")
    testthat::expect_equal(length(searches2), 1)
    testthat::expect_true("narrowpeak" %in% names(searches2))

    # Test custom searches
    custom <- list(myPeak = "custom_pattern")
    searches3 <- construct_searches(searches = custom)
    testthat::expect_equal(length(searches3), 1)
    testthat::expect_true("mypeak" %in% names(searches3))

    # Test case insensitivity of names
    custom2 <- list(NARROWPEAK = "test", BroadPeak = "test2")
    searches4 <- construct_searches(searches = custom2)
    testthat::expect_true(all(names(searches4) == tolower(names(searches4))))

    # Test error on invalid keys
    testthat::expect_error(
        construct_searches(keys = "nonexistent_key")
    )
})
