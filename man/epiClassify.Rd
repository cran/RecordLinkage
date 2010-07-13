\name{epiClassify}
\Rdversion{1.1}
\alias{epiClassify}
\title{
  Classify record pairs with EpiLink weights
}
\description{
  Classifies record pairs as link, non-link or possible link based on
  weights computed by \code{\link{epiWeights}} and the thresholds
  passed as arguments.
}

\usage{
epiClassify(rpairs, threshold.upper, threshold.lower = threshold.upper)
}

\arguments{
  \item{rpairs}{
    \code{\link{RecLinkData}} object. Record pairs to be classified.}
  \item{threshold.upper}{A numeric value between 0 and 1. }
  \item{threshold.lower}{A numeric value between 0 and 1 lower than \code{threshold.upper}}
}

\details{
  The object \code{rpairs} must have weights calculated by \code{\link{epiWeights}},
  stored in component \code{Wdata}. All record pairs with weights greater or
  equal \code{threshold.upper} are classified as links. Record pairs with
  weights smaller than \code{threshold.upper} and greater or equal
  \code{threshold.lower} are classified as possible links. All remaining
  records are classified as non-links.
}

\value{
  A \code{\link{RecLinkResult}} object with classification results stored in 
  component \code{prediction}, otherwise a copy of \code{rpairs}.
}

\author{Andreas Borg, Murat Sariyar}

\seealso{
  \code{\link{epiWeights}}
}

\examples{
# generate record pairs
data(RLdata500)
p=compare.dedup(RLdata500,strcmp=TRUE ,strcmpfun=levenshteinSim,
  identity=identity.RLdata500)

# calculate weights
p=epiWeights(p)

# classify and show results
summary(epiClassify(p,0.6))
}
\keyword{classif}
