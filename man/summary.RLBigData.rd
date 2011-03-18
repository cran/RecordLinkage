\name{summary.RLBigData}

\alias{summary.RLBigData}
\alias{summary,RLBigDataDedup-method}
\alias{summary,RLBigDataLinkage-method}
\alias{print.summaryRLBigDataDedup}
\alias{print.summaryRLBigDataLinkage}

\title{
  summary methods for \code{"RLBigData"} objects.
}
\description{
  Shows summarized information on a \code{"\linkS4class{RLBigDataDedup}"}
  or \code{"\linkS4class{RLBigDataDedup}"} object.
}

\usage{
  \S4method{summary}{RLBigDataDedup}(object)
  \S4method{summary}{RLBigDataLinkage}(object)
  \method{print}{summaryRLBigDataDedup}(x, ...)
  \method{print}{summaryRLBigDataLinkage}(x, ...)
}
\arguments{
  \item{object}{The object for which to show a summary.}
  \item{x}{Return value of the summary function.}
  \item{...}{Additional arguments from the generic function are ignored.}
}

\details{
  The \code{summary} methods return a list of the format shown below. The print
  method displays this information on the console in a user-friendly format.
  
  Blocking fields are displayed in a style like \samp{[attr1], [attr2, attr3]},
  where \samp{attr1} etc. are column names and attributes within brackets
  represent one blocking iteration. See \code{\link{compare.dedup}} or
  \code{\link{RLBigDataDedup}} for an explanation of blocking criteria.
}

\value{
  For \code{summary}, a list with components
  \item{nData}{Only for the \code{"\linkS4class{RLBigDataDedup}"} method:
    Number of records in the dataset.}
  \item{nData1}{Only for the \code{"\linkS4class{RLBigDataLinkage}"} method:
    Number of records in dataset 1.}
  \item{nData2}{Only for the \code{"\linkS4class{RLBigDataLinkage}"} method:
    Number of records in dataset 2.}
  \item{attributes}{Column names of dataset.}
  \item{blockFld}{Blocking definition as a list of character vectors,
    representing column names.}
  \item{expectedSize}{Approximate number of record pairs as calculated
    by \code{\link{getExpectedSize}}}
  \item{nMatches}{Number of matches in the set of record pairs.}
  \item{weightHist}{Only if weights have been calculated for \code{object}:
    a summary of the weights in histogram style.}
}

\author{
  Andreas Borg, Murat Sariyar
}

\seealso{
  \code{\link{summary}}
  \code{"\linkS4class{RLBigData}"}
  \code{RLBigDataDedup}, \code{RLBigDataLinkage}
}
\examples{
  data(RLdata500)
  rpairs <- RLBigDataDedup(RLdata500, blockfld=list(1,3,5:7))
  rpairs <- epiWeights(rpairs)
  summary(rpairs)
}
\keyword{methods}
