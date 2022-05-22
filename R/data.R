#' Metadata for epigenomic peaks: ROADMAP
#'
#' A subset of metadata from the Encyclopedia of DNA Elements
#'  (\href{https://www.encodeproject.org/}{ENCODE}) that only includes 
#'  entries for peaks (narrowPeak and broadPeak). 
#'  Metadata originally sourced from \link[AnnotationHub]{AnnotationHub} with
#'  additional post-processing and filtering. 
#' @source
#' \code{
#' meta <- search_roadmap()
#' peaks_metadata_roadmap <- meta[output_category=="peaks" & 
#'                                (!is.na(biosample_id)),]
#' usethis::use_data(peaks_metadata_roadmap, overwrite = TRUE)
#' }
#' @usage data("peaks_metadata_roadmap")
"peaks_metadata_roadmap"


#' Metadata for epigenomic peaks: AnnotationHub
#'
#' A subset of metadata from AnnotationHub that only includes 
#'  entries for peaks (narrowPeak and broadPeak).  
#' @source
#' \code{
#' meta <- search_annotationhub()
#' searches <- construct_searches(keys = c("broadpeak","narrowpeak","genericpeak"))
#' search_strings <- paste(unname(unlist(searches)),collapse = "|")
#' peaks_metadata_annotationhub <- meta[with(meta, 
#'                   grepl(search_strings, 
#'                         paste(title,description,sourcetype,sourceurl), 
#'                         ignore.case = TRUE)
#' ),
#' ]
#' drop_empty_cols(peaks_metadata_annotationhub)
#' usethis::use_data(peaks_metadata_annotationhub, overwrite = TRUE)
#' }
#' @usage data("peaks_metadata_annotationhub")
"peaks_metadata_annotationhub"
