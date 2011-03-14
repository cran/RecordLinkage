\name{getPairsBackend}
\alias{getPairsBackend}
%
\title{
  Backend function for getPairs
}
\description{
  Backend function for \code{\link{getPairs}} methods. Not to be called directly.
}
\usage{
getPairsBackend(object, filter.match, filter.link = c("nonlink", "possible", "link"), max.weight = Inf, min.weight = -Inf, withMatch = TRUE, withClass = FALSE, withWeight = FALSE, sort = FALSE, single.rows = FALSE)
}
\arguments{
  \item{object}{The object from which to extract pairs.}
  \item{filter.match}{Character vector, specifies matching status for pairs to appear in the output.}
  \item{filter.link}{Character vector, specifies linkage result for pairs to appear in the output.}
  \item{max.weight}{Maximum weight of pairs to include in the output.}
  \item{min.weight}{Minimum weight of pairs to include in the output.}
  \item{withMatch}{Logical. Wether to include matching status in the output.}
  \item{withClass}{Logical. Wether to include linkage result in the output.}
  \item{withWeight}{Logical. Wether to include weights in the output.}
  \item{sort}{Logical. Wether to sort descending by weights.}
  \item{single.rows}{Logical. Wether to print record pairs in one row instead
    of two consecutive rows.}
}

\details{
  See \code{\link{getPairs}}
}
\value{
  See \code{\link{getPairs}}
}

\author{
  Andreas Borg, Murat Sariyar
}

\keyword{internal}
