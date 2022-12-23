#' Create TrackHub
#' 
#' Create TrackHub for visualization of genomic files 
#' in the UCSC Genome Browser.
#' @param path Path to search for files within recursively.
#' @param folder Subfolders within \code{path} to search,
#' @param domain URL prefix where the files will be hosted.
#' @param filetypes File types to search for.
#' @param as_list Return each file as a nested list.
#' @param genome Genome build that files were aligned to.
#' @param save_path Path to to save \emph{trackDb} file to.
#' @returns Character or nested list.
#' @source \href{UCSC track metadata descriptions}{
#' https://genome-euro.ucsc.edu/goldenPath/help/trackDb/trackDbHub.html
#' }
#' @source \href{example trackhub}{
#' https://github.com/mhalushka/UCSC_miRNA_barchart
#' }
#' @source \href{https://github.com/lawremi/rtracklayer/issues/12}{
#' rtracklayer possible memory leakage
#' }
#' @export
#' @examples 
#' \dontrun{
#' path <-"/Volumes/bms20/projects/neurogenomics-lab/live/Projects/CUT_n_TAG/CUTnTag_analysis"
#' res <- create_trackhub(path=path, folders <- c("bigwig","bedgraph","peaks"))
#' }
create_trackhub <- function(hub="cutntag_benchmarking",
                            path=getwd(),
                            save_dir=file.path(path,"trackhub"),
                            folders=c(""), 
                            domain="https://webserver-schilder-ukdri.dsi.ic.ac.uk/cutntag_benchmarking",
                            shortLabel="CUT&Tag benchmarking",
                            longLabel="Data track associated with the CUT&Tag benchmarking manuscript: https://doi.org/10.1101/2022.03.30.486382",
                            email="",
                            visibility=2,
                            genome="hg19",
                            descriptionUrl=paste(save_dir,
                                                 "description.html",
                                                 sep="/"),
                            filetypes=c(#"bedGraph"=".bedgraph$|.bdg$",
                                        # "bed"=".bed$",
                                        "bigWig"=".bigwig$|.bw$"),
                            as_list=TRUE, 
                            sep="\t",
                            verbose=TRUE){
    
    # templateR:::args2vars(create_trackhub)
    # templateR:::source_all()
    # path="/Volumes/bms20/projects/neurogenomics-lab/live/Projects/CUT_n_TAG/CUTnTag_analysis"
    # folders <- c("bigwig","bedgraph","peaks")
    # filetypes=c("bedGraph"=".bedgraph$|.bdg$","bed"=".bed$","bigWig"=".bigwig$|.bw$")
    
    #### Create path names #### 
    descriptionUrl <- gsub(path,domain,descriptionUrl)
    genomesFile <- file.path(save_dir,"genomes.txt") 
    hubFile  <- file.path(save_dir,"hub.txt")
    trackdbFile <- file.path(save_dir,genome,"trackDb.txt")
    ucsc_url <- paste0(
        "http://genome.ucsc.edu/cgi-bin/hgTracks",
        "?db=",genome,
        "&hubUrl=",domain,top_dir(path = hubFile, n=2, invert = TRUE)
    )  
    #### Create genomes.txt #### 
    genomes_lst <- create_trackhub_genomes(genomesFile=genomesFile,
                                           trackdbFile=trackdbFile,
                                           genome=genome,
                                           verbose=verbose) 
    #### Create hub.txt ####
    hub_lst <- create_trackhub_hub(hub=hub,
                                   descriptionUrl=descriptionUrl,
                                   shortLabel=shortLabel,
                                   longLabel=longLabel,
                                   email=email,
                                   ucsc_url=ucsc_url,
                                   hubFile=hubFile,
                                   genomesFile=genomesFile,
                                   as_list=as_list,
                                   verbose=verbose)
    #### Create trackDb.txt ####
    trackdb_lst <- create_trackhub_trackdb(trackdbFile=trackdbFile,
                                           path=path,
                                           hub=hub,
                                           domain=domain,
                                           genome=genome,
                                           folders=folders,
                                           filetypes=filetypes, 
                                           visibility=visibility,
                                           as_list=as_list,
                                           sep=sep,
                                           verbose=verbose) 
    return(list(genomes=genomes_lst,
                hub=hub_lst,
                trackdb=trackdb_lst,
                ## Paths
                genomesFile=genomesFile,
                hubFile=hubFile,
                trackdbFile=trackdbFile,
                ucsc_url=ucsc_url
    ))
}

# f <- system.file("tests", "trackhub", package = "rtracklayer")
# readLines(list.files(f, full.names = TRUE)[[1]])
# 
# th <- rtracklayer::TrackHub(system.file("tests", "trackhub", package = "rtracklayer"))
# 
# th <- rtracklayer::TrackHub(uri = f, create = F)
# th@uri <- "https://webserver-schilder-ukdri.dsi.ic.ac.uk/cutntag_benchmarking"
# th@hub <- "cutntag_benchmarking"
# th@shortLabel <- "CUT&Tag benchmarking"
# th@longLabel <- "Data track associated with the CUT&Tag benchmarking manuscript: https://doi.org/10.1101/2022.03.30.486382"
# 
# rtracklayer::writeTrackHub(x = th) 