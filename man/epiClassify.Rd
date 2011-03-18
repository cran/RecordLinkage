\name{epiClassify}
\Rdversion{1.1}
\alias{epiClassify}
\alias{epiClassify-methods}
\alias{epiClassify,RLBigData-method}
\alias{epiClassify,RecLinkData-method}

\title{
  Classify record pairs with EpiLink weights
}
\description{
  Classifies record pairs as link, non-link or possible link based on
  weights computed by \code{\link{epiWeights}} and the thresholds
  passed as arguments.
}

\usage{
epiClassify(rpairs, threshold.upper, threshold.lower = threshold.upper,
  ...)

\S4method{epiClassify}{RecLinkData}(rpairs, threshold.upper, threshold.lower = threshold.upper)

\S4method{epiClassify}{RLBigData}(rpairs, threshold.upper, threshold.lower = threshold.upper,
  e = 0.01, f = getFrequencies(rpairs), withProgressBar = (sink.number()==0))
}

\arguments{
  \item{rpairs}{
    \code{\link{RecLinkData}} object. Record pairs to be classified.}
  \item{threshold.upper}{A numeric value between 0 and 1. }
  \item{threshold.lower}{A numeric value between 0 and 1 lower than \code{threshold.upper}}
  \item{e}{Numeric vector. Estimated error rate(s).}
  \item{f}{Numeric vector. Average frequency of attribute values.}
  \item{withProgressBar}{Logical. Whether to display a progress bar.}
  \item{...}{Placeholder for optional arguments}
}

\details{
  All record pairs with weights greater or
  equal \code{threshold.upper} are classified as links. Record pairs with
  weights smaller than \code{threshold.upper} and greater or equal
  \code{threshold.lower} are classified as possible links. All remaining
  records are classified as non-links.

  For the \code{"RecLinkData"} method, weights must have been calculated
  for \code{rpairs} using \code{\link{epiWeights}}.

  The \code{"RLBigData"}
  method checks if weights are present in the underlying database.
  If this is the case, classification
  is based on the existing weights. If not, weights are calculated on the fly
  during classification, but not stored. The latter behaviour might be preferable
  when a very large dataset is to be classified or disk space is limited
  (see also the notes to \code{\link{epiWeights}}).

  A progress bar is displayed by the \code{"RLBigData"} method only if
  weights are calculated on the fly and, by default, unless output is diverted by
  \code{\link{sink}} (e.g. in a Sweave script).
}

\value{
  For the \code{"\link{RecLinkData}"} method, a S3 object
  of class \code{"\link{RecLinkResult}"} that represents a copy
  of \code{newdata} with element \code{rpairs$prediction}, which stores
  the classification result, as addendum.

  For the \code{"\linkS4class{RLBigData}"} method, a S4 object of class
  \code{"\linkS4class{RLResult}"}.
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
