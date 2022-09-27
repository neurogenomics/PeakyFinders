#' Example BAM files
#' 
#' Download exampe BAM files from 
#' \href{nf-core}{https://nf-co.re/cutandrun/results}.
#' @param save_dir Directory to save files to.
#' @export
#' @importFrom utils download.file
#' @examples 
#' bam_files <- example_bam()
example_bam <- function(save_dir=tempdir()){
    baseurl <- paste(
        "https://nf-core-awsmegatests.s3-eu-west-1.amazonaws.com",
        "cutandrun/results-971984a48ad4dc5b39fc20b56c5729f4ca20379a",
        "02_alignment/bowtie2/target",sep="/") 
    urls <- paste(baseurl,
                  c("igg_ctrl_R1.target.dedup.bam",
                    "igg_ctrl_R2.target.dedup.bam"), sep="/")
    files <- lapply(urls,function(x){
        f <- file.path(save_dir,basename(x))
        if(file.exists(f)) return(f)
        utils::download.file(x,f)
        return(f)
    })
    return(files)
}
