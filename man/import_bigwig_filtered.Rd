% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_bigwig_filtered.R
\name{import_bigwig_filtered}
\alias{import_bigwig_filtered}
\title{Import filtered bigwig}
\usage{
import_bigwig_filtered(bw.file, gr.query_dat, full_data = TRUE)
}
\arguments{
\item{bw.file}{Path to a bigwig file.}

\item{gr.query_dat}{\link[GenomicRanges]{GRanges}
object to query the bigwig file with.}

\item{full_data}{Whether to return the actual read ranges
 (\code{full_data=TRUE}),
or just the "score" column which summarizes the height of
the aggregated reads across the genome (\code{full_data=TRUE}).}
}
\description{
Import a subset of a bigwig file
based on the coordinates in a \link[GenomicRanges]{GRanges} 
object (\code{gr.query_dat}).
}
\keyword{internal}
