\name{unorderedPairs}
\alias{unorderedPairs}

\title{Create Unordered Pairs}
\description{
  Creates all unordered pairs of some objects or of the first
  \code{x} natural numbers.  
}
\usage{
unorderedPairs(x)
}
\arguments{
  \item{x}{Either an arbitrary vector or a natural number}
}
\details{
  If \code{x} has length one, all unordered pairs of the first \code{x}
  natural numbers are created. If \code{x} has more than one element,
  all unordered pairs of the elements of \code{x} are created.
}
\value{
  A matrix with two rows, each column holding one pair.
}
\author{Andreas Borg}

\examples{
  # create unordered pairs of {1,2,3}: {1,2},{1,3} and {2,3}
  unorderedPairs(3)
  # create unordered pairs of {"a","b","c"}: {"a","b"}, {"a","c"},{"b","c"}
  unorderedPairs(c("a","b","c"))
}
\keyword{misc}
