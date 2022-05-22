test_that("import_peaks works", {
    
    #### setup ####
    query_granges <- GenomicRanges::GRanges("chr6:165169213-167169213")
    

    #### genericPeak: Without query_granges ####
    ids <- "GSM2101439" 
    grl <- PeakyFinders::import_peaks(ids = ids,
                                       builds = "hg19",
                                       searches = list(genericPeak="peak"))
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 96376)
    remove(grl)
    
    #### genericPeak: With query_granges #### 
    ids <- "GSM2101439" 
    grl <- PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   searches = list(genericPeak="peak"))
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 68)
    remove(grl)
    
    
    #### broadPeak: With query_granges #### 
    ids <- "GSM1003455"
    grl <- PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   searches = list(broadPeak="broadpeak"))
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 3)
    remove(grl)
    
    
    #### narrowPeak: With query_granges #### 
    ## Import from both GEO, ENCODE, ROADMAP, AnnotationHub
    ids <- c("GSM945244", # GEO
             "ENCSR000AHD", # ENCODE
             "AH32001",#"E001" # ROADMAP
             "AH22394" # AnnotationHub
             ) 
    query_granges <- get_genome(keep.chr = "chr22", genome = "hg38")
    grl_out <- PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg38",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   searches = list(narrowPeak="narrowpeak"))
    for(nm in names(grl_out)){
        grl <- grl_out[[nm]] 
        testthat::expect_true(methods::is(grl[[1]], "GRanges"))
        if(nm=="GEO") {
            testthat::expect_true(names(grl)==ids[1])
            testthat::expect_length(grl[[1]], 2417)
        }
        if(nm=="ENCODE") {
            testthat::expect_true(names(grl)==ids[2])
            testthat::expect_length(grl[[1]], 13109)
        } 
        if(nm=="ROADMAP") {
            testthat::expect_true(names(grl)==ids[3])
            testthat::expect_length(grl[[1]], 881)
        } 
        if(nm=="AnnotationHub") {
            testthat::expect_true(names(grl)==ids[4])
            testthat::expect_length(grl[[1]], 5064)
        } 
    } 
    remove(grl_out, grl, nm)
    
    
    #### called peaks: Without query_granges #### 
    ids <- "GSM4703766"
    grl <- PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19", 
                                   searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                        bigWig="bigwig|bw$"))
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 18112)
    remove(grl)
    
    
    #### called peaks: With query_granges #### 
    ids <- "GSM4703766"
    grl <- PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg19",
                                   query_granges = query_granges, 
                                   query_granges_build = "hg38",
                                   searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                        bigWig="bigwig|bw$"))
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 304)
    remove(grl)
    
    
    #### called peaks from bigWig: With query_granges ####  
    ids <- "GSM5684359" 
    query_granges <- get_genome(keep.chr = "chr4", genome = "hg38")
    grl <- PeakyFinders::import_peaks(ids = ids,
                                   builds = "hg38",
                                   query_granges = query_granges,
                                   query_granges_build = "hg38",
                                   searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                        bigWig="bigwig|bw$"))
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 4)
    remove(grl)
    
    
    #### called peaks from bigWig:  split_chromosomes ####  
    ids <- "GSM5684359" 
    ## Query using the same genome build whenever possible
    ## because liftover tends to distribute 
    ## regions all over the genome.
    query_granges <- PeakyFinders::get_genome(genome = "hg38",
                                              keep.chr=20:22)  
    grl <- PeakyFinders::import_peaks(ids = ids,
                                      builds = "hg38", 
                                      query_granges = query_granges,
                                      query_granges_build = "hg38",
                                      searches = list(bedGraph="bedgraph|graph.gz|bdg.gz",
                                                           bigWig="bigwig|bw$"),
                                      split_chromosomes = TRUE, 
                                      nThread = 1)
    grl <- grl$GEO
    testthat::expect_true(names(grl)==ids)
    testthat::expect_true(methods::is(grl[[1]], "GRanges"))
    testthat::expect_length(grl[[1]], 16)
    remove(grl)
})
