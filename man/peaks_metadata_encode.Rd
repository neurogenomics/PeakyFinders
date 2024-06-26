% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peaks_metadata_encode.R
\name{peaks_metadata_encode}
\alias{peaks_metadata_encode}
\title{Get ENCODE peaks metadata}
\source{
\code{
### Get peak data only #####
meta <- search_encode(file_type = c("^bed narrowpeak","^bed broadpeak"),
                      assembly = "GRCh38",
                      organism = "Homo sapiens",
                      partial_match = TRUE)
meta <- subset(meta[(!file_type %in% c("vcf","hic")),])
PeakyFinders:::drop_empty_cols(meta)
tmp <- "~/Downloads/encode_peaks_metadata.tsv.gz"
data.table::fwrite(meta, file = tmp, sep="\t")
}
}
\usage{
peaks_metadata_encode(
  save_dir = tools::R_user_dir(package = "PeakyFinders", which = "cache"),
  overwrite = FALSE
)
}
\arguments{
\item{save_dir}{Directory to cache file in.}

\item{overwrite}{Should any local files of the same name be overwritten?
default \code{TRUE}.}
}
\value{
\link[data.table]{data.table}.
}
\description{
A subset of the ENCODE metadata that includes links to all
 human epigenomic peaks data.
}
\examples{
meta <- peaks_metadata_encode()
}
