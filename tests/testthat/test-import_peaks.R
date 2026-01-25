test_that("import_peaks works", {

    ## Skip on CRAN since tests depend on external GEO/ENCODE servers
    testthat::skip_on_cran()
    testthat::skip_if_offline()

    #### setup ####
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")


    #### genericPeak: Without query_granges ####
    ids <- "GSM2101439"
    grl <- tryCatch({
        PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19",
                                   searches = list(genericPeak="peak"))
    }, error = function(e) {
        testthat::skip(paste("GEO import failed:", e$message))
    })
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_true(length(grl[[1]]) > 0)
    remove(grl)
    
    #### genericPeak: With query_granges ####
    ids <- "GSM2101439"
    grl <- tryCatch({
        PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges,
                                   query_granges_build = "hg38",
                                   searches = list(genericPeak="peak"))
    }, error = function(e) {
        testthat::skip(paste("GEO import with query_granges failed:", e$message))
    })
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_true(length(grl[[1]]) > 0)
    remove(grl)
    
    
    #### broadPeak: With query_granges ####
    ids <- "GSM1003455"
    grl <- tryCatch({
        PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges,
                                   query_granges_build = "hg38",
                                   searches = list(broadPeak="broadpeak"))
    }, error = function(e) {
        testthat::skip(paste("GEO broadPeak import failed:", e$message))
    })
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_true(length(grl[[1]]) > 0)
    remove(grl)
    
    
    #### narrowPeak: With query_granges ####
    ## Import from both GEO, ENCODE, ROADMAP, AnnotationHub
    ids <- c("GSM945244", # GEO
             "ENCSR000AHD", # ENCODE
             "AH32001",#"E001" # ROADMAP
             "AH22394" # AnnotationHub
             )
    query_granges <- get_genome(keep.chr = "chr22", genome = "hg38")
    grl_out <- tryCatch({
        PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg38",
                                   query_granges = query_granges,
                                   query_granges_build = "hg38",
                                   searches = list(narrowPeak="narrowpeak"))
    }, error = function(e) {
        testthat::skip(paste("Multi-source import failed:", e$message))
    })
    for(nm in names(grl_out)){
        grl <- grl_out[[nm]]
        testthat::expect_true(methods::is(grl[[1]], "GRanges"))
        testthat::expect_true(length(grl[[1]]) > 0)
    }
    remove(grl_out, grl, nm)
    
    
    #### called peaks: Without query_granges ####
    bg_test <- function(){
        ids <- "GSM4703766"
        grl <- tryCatch({
            PeakyFinders::import_peaks(ids = ids,
                                       builds = "hg19",
                                       searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                       bigWig="bigwig|bw$"))
        }, error = function(e) {
            testthat::skip(paste("bedGraph import failed:", e$message))
        })
        grl <- grl$GEO
        testthat::expect_true(names(grl)==ids)
        testthat::expect_true(methods::is(grl[[1]], "GRanges"))
        testthat::expect_true(length(grl[[1]]) > 0)
        remove(grl)
    }
    if(.Platform$OS.type=="windows"){
        testthat::skip("MACSr is incompatible with Windows")
    } else {
        bg_test()
    }
    
    
    #### called peaks: With query_granges ####
    bg_test2 <- function(){
        ids <- "GSM4703766"
        grl <- tryCatch({
            PeakyFinders::import_peaks(ids = ids,
                                       builds = "hg19",
                                       query_granges = query_granges,
                                       query_granges_build = "hg38",
                                       searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                       bigWig="bigwig|bw$"))
        }, error = function(e) {
            testthat::skip(paste("bedGraph import with query_granges failed:", e$message))
        })
        grl <- grl$GEO
        testthat::expect_true(names(grl)==ids)
        testthat::expect_true(methods::is(grl[[1]], "GRanges"))
        testthat::expect_true(length(grl[[1]]) > 0)
        remove(grl)
    }
    if(.Platform$OS.type=="windows"){
        testthat::skip("MACSr is incompatible with Windows")
    } else {
        bg_test2()
    }
    
    
    #### called peaks from bigWig: With query_granges ####
    bw_test <- function(){
        ids <- "GSM5684359"
        query_granges <- get_genome(keep.chr = "chr4", genome = "hg38")
        grl <- tryCatch({
            PeakyFinders::import_peaks(ids = ids,
                                       builds = "hg38",
                                       query_granges = query_granges,
                                       query_granges_build = "hg38",
                                       searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                       bigWig="bigwig|bw$"))
        }, error = function(e) {
            testthat::skip(paste("bigWig import failed:", e$message))
        })
        grl <- grl$GEO
        testthat::expect_true(names(grl)==ids)
        testthat::expect_true(methods::is(grl[[1]], "GRanges"))
        testthat::expect_true(length(grl[[1]]) > 0)
        remove(grl)
    }
    if(.Platform$OS.type=="windows"){
        testthat::skip("MACSr is incompatible with Windows")
    } else {
        bw_test()
    }
    
    
    bw_test2 <- function(){
        #### called peaks from bigWig:  split_chromosomes ####
        ids <- "GSM5684359"
        ## Query using the same genome build whenever possible
        ## because liftover tends to distribute
        ## regions all over the genome.
        query_granges <- PeakyFinders::get_genome(genome = "hg38",
                                                  keep.chr=20:22)
        grl <- tryCatch({
            PeakyFinders::import_peaks(ids = ids,
                                       builds = "hg38",
                                       query_granges = query_granges,
                                       query_granges_build = "hg38",
                                       searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                       bigWig="bigwig|bw$"),
                                       split_chromosomes = TRUE,
                                       nThread = 1)
        }, error = function(e) {
            testthat::skip(paste("bigWig split_chromosomes import failed:", e$message))
        })
        grl <- grl$GEO
        testthat::expect_true(names(grl)==ids)
        testthat::expect_true(methods::is(grl[[1]], "GRanges"))
        testthat::expect_true(length(grl[[1]]) > 0)
        remove(grl)
    }

    if(.Platform$OS.type=="windows"){
        testthat::skip("MACSr is incompatible with Windows")
    } else {
        bw_test2()
    }
    
    #### Test LOCAL file import ####
    ids <- system.file("tests/test.bed", package = "rtracklayer")
    grl <- tryCatch({
        import_peaks(ids = ids,
                     builds = "hg19")
    }, error = function(e) {
        testthat::skip(paste("Local file import failed:", e$message))
    })
    grl <- grl$file
    testthat::expect_true(names(grl)==basename(ids))
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 5)
    remove(grl)

    #### Test REMOTE file import ####
    ids <- "https://webserver-schilder-ukdri.dsi.ic.ac.uk/cutntag_benchmarking/peaks/rmDup/Abcam-ab4729_MACS2.bed"
    grl <- tryCatch({
        import_peaks(ids = ids,
                     builds = "hg19")
    }, error = function(e) {
        testthat::skip(paste("Remote file import failed:", e$message))
    })
    grl <- grl$file
    testthat::expect_true(names(grl)==basename(ids))
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_true(length(grl[[1]]) > 0)
    remove(grl)
})
