% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_chrom.R
\name{filter_chrom}
\alias{filter_chrom}
\title{Filter chromosomes}
\usage{
filter_chrom(
  grlist,
  keep_chr = paste0("chr", c(seq_len(22), "X", "Y")),
  verbose = TRUE
)
}
\arguments{
\item{grlist}{Named list of \link[GenomicRanges]{GRanges} objects.}

\item{keep_chr}{Which chromosomes to keep.}

\item{verbose}{Print messages.}
}
\value{
Named list of \link[GenomicRanges]{GRanges} objects.
}
\description{
Remove non-standard chromosomes from a list of 
\link[GenomicRanges]{GRanges} objects.
}
\examples{
gr <- GenomicRanges::GRanges(c("chr6:1-1000","chrUns999:1-1000"))
gr2 <- filter_chrom(grlist=gr)
}
