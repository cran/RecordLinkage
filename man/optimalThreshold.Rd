\name{optimalThreshold}
\Rdversion{1.1}
\alias{optimalThreshold}

\title{
  Optimal Threshold for Record Linkage
}
\description{
  Calculates the optimal threshold for weight-based Record Linkage.
}
\usage{
optimalThreshold(rpairs, my = NaN, ny = NaN)
}
\arguments{
  \item{rpairs}{
    \code{\link{RecLinkData}} object. Record Pairs for which to calculate
    a threshold.
}
  \item{my}{
    A probability. Error bound for false posicives.
}
  \item{ny}{
    A probability. Error bound for false negatives.
}
}
\details{
  \code{rpairs} must contain weights in \code{rpairs$Wdata}, calculated by
  a suitable function such as \code{\link{emWeights}} or 
  \code{\link{epiWeights}}. The true match result must be known for
  \code{rpairs}.
  
  For the following it is assumed that all records with weights greater than or
  equal to the threshold are classified as links, the remaining as non-links.
  If no further arguments are given, a threshold which minimizes the
  absolute number of misclassified record pairs is returned. If \code{my} is 
  supplied (\code{ny} is ignored in this case), a threshold is picked which
  maximizes the number of correctly classified links while keeping the ratio 
  of false links to the total number of links below or equal \code{my}.
  If \code{ny} is supplied, the number of correct non-links is maximized under the
  condition that the ratio of falsely classified non-links to the total number of
  non-links does not exceed \code{ny}.
  
  Two seperate runs of \code{optimalThreshold} with values for \code{my} and
  \code{ny} respectively allow for obtaining a lower and an upper threshold
  for a three-way classification approach (yielding links, non-links and
  possible links).
}
\value{
  A numeric value, the calculated threshold.
}

\author{
  Andreas Borg
}

\seealso{
  \code{\link{emWeights}}
  \code{\link{emClassify}}
  \code{\link{epiWeights}}
  \code{\link{epiClassify}}
}
\examples{
# create record pairs
data(RLdata500)
p=compare.dedup(RLdata500,identity=identity.RLdata500, strcmp=TRUE,
  strcmpfun=levenshteinSim)

# calculate weights
p=epiWeights(p)

# split record pairs in two sets
l=splitData(dataset=p, prop=0.5, keep.mprop=TRUE)

# get threshold from training set
threshold=optimalThreshold(l$train)

# classify remaining data
summary(epiClassify(l$valid,threshold))
}

\keyword{classif}

