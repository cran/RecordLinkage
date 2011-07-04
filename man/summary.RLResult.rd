\name{summary.RLResult}

\alias{summary.RLResult}
\alias{summary,RLResult-method}
\alias{print.summaryRLResult}

\title{
  Summary method for \code{"RLResult"} objects.
}
\description{
  Get summarized information on a \code{"\linkS4class{RLResult}"}
  object.
}

\usage{
  \S4method{summary}{RLResult}(object)
  \method{print}{summaryRLResult}(x, ...)
}

\arguments{
  \item{object}{The object for which to show a summary.}
  \item{x}{Return value of the summary function.}
  \item{...}{Additional arguments from the generic function are ignored.}
}

\details{
  The \code{summary} methods return a list of the format shown below. The print
  method displays this information on the console in a user-friendly format.
  
}

\value{
  For \code{summary}, a list with components
  \item{nPairs}{Number of record pairs.}
  \item{nLinks}{Number of detected links.}
  \item{nPossibleLinks}{Number of detected possible links.}
}

\author{
  Andreas Borg, Murat Sariyar
}

\seealso{
  \code{\link{summary}}
  \code{"\linkS4class{RLResult}"}
}
\examples{
  data(RLdata500)
  rpairs <- RLBigDataDedup(RLdata500, blockfld=list(1,3,5:7),
    identity = identity.RLdata500)
  rpairs <- epiWeights(rpairs)
  result <- epiClassify(rpairs, 0.7)
  summary(result)
}
\keyword{methods}
