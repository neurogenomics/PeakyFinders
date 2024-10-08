% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_peaks.R
\name{call_peaks}
\alias{call_peaks}
\title{Call peaks}
\usage{
call_peaks(
  bedgraph_path,
  method = c("MACSr", "SEACR"),
  cutoff = NULL,
  minlen = 200L,
  maxgap = 30L,
  call_summits = TRUE,
  trackline = TRUE,
  log = TRUE,
  norm = TRUE,
  stringent = TRUE,
  outdir = tempdir(),
  outputfile = paste0(method[[1]], ".peaks.bed"),
  return_path = FALSE,
  nThread = 1,
  verbose = TRUE
)
}
\arguments{
\item{bedgraph_path}{Path to bedGraph file. 
Can instead provide a bigWig file, 
but this will first be converted to bedGraph format, 
which can take some time if trying to convert data from across the entire
genome.}

\item{method}{Method to call peaks with:
\itemize{
\item{"MACSr" : }{Uses \href{https://github.com/macs3-project/MACS}{MACS3} 
via \link[MACSr]{bdgpeakcall}.}
\item{"SEACR" : }{Uses \href{https://github.com/FredHutch/SEACR}{SEACR} 
via \link[echoconda]{find_packages}.}
}}

\item{cutoff}{\itemize{
\item{when \code{method="MACSr"} : }{
Passed to \code{cutoff} argument.
Cutoff depends on which method you used for score track.
 If the file contains pvalue scores from MACS3, score 5 means pvalue 1e-5.
 If \code{NULL}, a reasonable \code{cutoff} value will be inferred 
 through a \code{cutoff_analysis}. 
}
\item{when \code{method="SEACR"} : }{
Passed to \code{control} argument.
Control (IgG) data bedgraph file to generate an empirical
threshold for peak calling.
Alternatively, a numeric threshold n between 0 and 1 returns the top n
fraction of peaks based on total signal within peaks 
(default: \code{0.05}).
}
}}

\item{minlen}{minimum length of peak, better to set it as d
value. DEFAULT: 200", default = 200.}

\item{maxgap}{maximum gap between significant points in a peak,
better to set it as tag size. DEFAULT: 30", default = 30.}

\item{call_summits}{If set, MACS will use a more sophisticated
approach to find all summits in each enriched peak region
DEFAULT: False",default=False.}

\item{trackline}{Tells MACS not to include trackline with bedGraph files. The trackline is used by UCSC for the options for display.}

\item{log}{Whether to capture logs.}

\item{norm}{Field 3: "norm" (default: \code{norm=TRUE}) 
denotes normalization of control 
to target data, "non" (\code{norm=FALSE}) skips this behavior.
"norm" is recommended unless experimental and control data are already 
rigorously normalized to each other (e.g. via spike-in).}

\item{stringent}{Field 4:"relaxed" (\code{stringent=FALSE})
uses a total signal 
threshold between the knee and peak of the total signal curve,
and corresponds to the “relaxed” mode described in the text, 
whereas “stringent” (default: \code{stringent=TRUE}) 
uses the peak of the curve, and corresponds to “stringent” mode.}

\item{outdir}{Directory to store \code{cutoff_analysis} 
report and peak file in.}

\item{outputfile}{Name of the peak output file (stored in BED format).}

\item{return_path}{Whether to return the path to the saved peak file, 
or the peak data itself as a \link[GenomicRanges]{GRanges} object.}

\item{nThread}{When \code{nThread>1}, accelerates file importing 
and peak calling using multi-core parallelisation.}

\item{verbose}{Print messages.}
}
\value{
\link[GenomicRanges]{GRanges} or path to save peaks file.
}
\description{
Call peaks from a bedGraph or bigWig file using multiple methods.
By default, it automatically infers a reasonable 
\code{cutoff} threshold as well.\cr
\emph{Note : } 
\pkg{MACS3}/\pkg{MACSr} is not currently compatible with Windows 
(see \href{https://github.com/macs3-project/MACSr/issues/13}{here}
 for details).
}
\examples{
files <- example_bg_bw()
peaks <- PeakyFinders::call_peaks(bedgraph_path = files$bedgraph,
                                  method="SEACR")
}
