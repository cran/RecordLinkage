\name{subset}
\alias{[.RecLinkData}
\alias{[.RecLinkResult}

\title{
  Subset operator for record linkage objects
}
\description{
  Extracts a subset of a \code{"\link{RecLinkData}"} or
  \code{"\link{RecLinkResult}"} object.
}
\usage{
  \method{[}{RecLinkData}(x, i)

  \method{[}{RecLinkResult}(x, i)
}

\arguments{
  \item{x}{
  The object which to index.
}
  \item{i}{
  Indeces of pairs to include in the subset.
}
}

\value{
  A copy of \code{x} with only the pairs with indices specified by \code{x}.
}
\author{
  Andreas Borg, Murat Sariyar
}
\examples{

## Samples a subset of pairs

data(RLdata500)
rpairs <- compare.dedup(RLdata500, identity = identity.RLdata500,
  blockfld = list(1,3,5,6,7))
nPairs <- nrow(rpairs$pairs)
s <- sample(nPairs, nPairs / 2)
samp <- rpairs[s]
}

\keyword{classif }
