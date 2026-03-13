test_that("translate_genome works", {

    # Test UCSC to Ensembl
    genome <- translate_genome(genome = "hg38", style = "Ensembl")
    testthat::expect_equal(genome, "GRCh38")

    # Test hg19
    genome2 <- translate_genome(genome = "hg19", style = "Ensembl")
    testthat::expect_equal(genome2, "GRCh37")

    # Test Ensembl to UCSC
    genome3 <- translate_genome(genome = "GRCh38", style = "UCSC")
    testthat::expect_equal(genome3, "hg38")

    # Test identity (UCSC to UCSC)
    genome4 <- translate_genome(genome = "hg38", style = "UCSC")
    testthat::expect_equal(genome4, "hg38")

    # Test invalid genome throws error
    testthat::expect_error(
        translate_genome(genome = "invalid_genome", style = "UCSC")
    )
})
