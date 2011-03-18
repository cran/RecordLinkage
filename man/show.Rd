\name{show}

\alias{show}
\alias{show,RLBigData-method}

\title{
  Show a RLBigData object
}
\description{
  Shows summarized information on a \code{"\linkS4class{RLBigData}"} object.
}

\usage{
  \S4method{show}{RLBigData}(object)
}
\arguments{
  \item{object}{The object for which to show a summary.}
}
\details{
   The printed information consists of the type of linkage procedure
   (deduplication or linkage) and the number of records and the approximate
   number of record pairs as calculated by \code{\link{getExpectedSize}}.
   More information is obtained by the
   \code{\link[=summary,RLBigDataDedup-method]{summary}} methods for these classes.
}


\value{
  \code{show} returns an invisible \code{NULL} and is used for its side effect.
}

\author{
  Andreas Borg, Murat Sariyar
}

\seealso{
  \code{\link[methods]{show}}
}
\examples{
  data(RLdata500)
  rpairs <- RLBigDataDedup(RLdata500)
  rpairs
}
\keyword{methods}
