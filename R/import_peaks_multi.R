#' Import peaks: multi
#'
#' Automatically import peaks in different file formats.
#' If the file is in \emph{bigWig} or \emph{bedGraph} format, 
#' peaks wil be called first.
#' 
#' @returns \link[GenomicRanges]{GRanges}
#' 
#' @inheritParams import_peaks 
#' @inheritParams construct_searches 
#' @keywords internal
#' @importFrom BiocParallel register SnowParam bplapply
import_peaks_multi <- function(links,
                               id=basename(tempfile()),
                               build = "hg19",
                               query_granges = NULL,
                               query_granges_build = NULL,
                               split_chromosomes = FALSE,
                               condense_queries = TRUE,
                               call_peaks_method = "MACSr",
                               cutoff = NULL,
                               searches = construct_searches(),
                               peaks_dir = tempdir(),
                               merge_list = TRUE,
                               nThread = 1,
                               verbose = TRUE){
    #### Parallelise ####
    ## SnowParam is less prone to errors than MultiParam, 
    ## but doesn't work on Windows.
    t1 <- Sys.time() 
    if(!is.null(names(links))){
        names(links) <- tolower(names(links))
    }
    #### Liftover (if necessary) ####
    if(!is.null(query_granges)){
        query_granges <- liftover_grlist(grlist = query_granges, 
                                         input_build = query_granges_build,
                                         output_build = build,
                                         style = "UCSC") 
        query_granges_build <- build
    }
    #### Split up chromosomes ####
    if(split_chromosomes){ 
        if(is.null(query_granges)){
            query_granges <- get_genome(genome = build)
        }  
        ## Condense granges to 1/chromosome 
        ## to reduce the number of queries.
        query_granges <- condense_granges(gr = query_granges)
        query_granges_list <- split_chromosomes_run(
            query_granges = query_granges
        )
    } else {
        query_granges_list <- list(all=query_granges)
    }  
    #### Iterate over chromosomes #### 
    BPPARAM <- get_bpparam(workers = nThread)
    peaks_all <- BiocParallel::bplapply(X = query_granges_list, 
                                        BPPARAM = BPPARAM,
                                        FUN = function(query_granges){
        message("\n")                               
        #### Intialize GRanges object ####                                             
        peaks_l <- GenomicRanges::GRanges()
        #### Determine which chrom is being queried #### 
        if(nThread>1 && split_chromosomes) {
            chrom <- unique(GenomicRanges::seqnames(query_granges))
            message_parallel("Processing: ",paste(chrom,collapse = ",")) 
        }
        ## Use this bc right now "narrowPeaks.tsv" would be imported by both
        ## narrowpeak/genericpeak functions.  
        processed <- FALSE;
        
        #### If files include narrowPeak, import directly #### 
        if(length(links$narrowpeak)>0){
            peaks <- import_peaks_narrowpeak(paths = links$narrowpeak,
                                             query_granges = query_granges,
                                             verbose = verbose)
            processed <- TRUE
            peaks_l <- c(peaks_l, peaks)
        } 
        #### If files include broadPeak, import directly #### 
        if(length(links$broadpeak)>0){
            peaks <- import_peaks_broadpeak(paths = links$broadpeak,
                                            query_granges = query_granges,
                                            verbose = verbose)
            processed <- TRUE
            peaks_l <- c(peaks_l, peaks)
        }  
        #### If files include generic peaks, import directly #### 
        if(length(links$genericpeak)>0 && isFALSE(processed)){ 
            nThread_dt <- if(split_chromosomes) 1 else nThread
            peaks <- import_peaks_genericpeak(paths = links$genericpeak,
                                              nThread = nThread_dt,
                                              verbose = verbose)
            peaks_l <- c(peaks_l, peaks) 
        }
        
        #### Call peaks from bedGraph #### 
        if(length(links$bedgraph)>0){
            messager("Computing peaks from bedGraph file.",v=verbose)
            peaks <- import_peaks_bedgraph(paths=links$bedgraph, 
                                           id=id, 
                                           query_granges=query_granges,
                                           build=build,
                                           call_peaks_method=call_peaks_method,
                                           cutoff=cutoff,
                                           peaks_dir=peaks_dir,
                                           verbose=verbose)
            peaks_l <- c(peaks_l, peaks)
        }
        
        #### Call peaks from bigWig #### 
        if(length(links$bigwig)>0){
            peaks <- import_peaks_bigwig(paths=links$bigwig,
                                         id=id,
                                         query_granges=query_granges,
                                         build=build,
                                         call_peaks_method=call_peaks_method,
                                         cutoff=cutoff,
                                         peaks_dir=peaks_dir,
                                         verbose=verbose)
            peaks_l <- c(peaks_l, peaks)  
        } 
        #### Post-processing ####
        if(length(peaks_l)==0) {
            message_parallel("No peaks identified.")
            # Skip filtering if empty
            return(peaks_l)
        }
        #### Subset peaks ####
        if(!is.null(query_granges)){
            peaks_l <- subset_peaks(peaks = peaks_l,
                                    query_granges = query_granges)
        } 
        message("\n")
        return(peaks_l)
    })  # <-- End parallel loop
    
    #### Merge list ####
    if(merge_list){
        peaks_all <- unlist(GenomicRanges::GRangesList(peaks_all)) 
        #### Ensure all rows are unique ####
        if(length(unique(names(peaks_all)))==1){
            names(peaks_all) <- NULL
        } else {
            names(peaks_all) <- make.unique(names(peaks_all))
        } 
    } 
    t2 <- Sys.time()
    difftime(t2, t1, units = "min")
    return(peaks_all)
}
