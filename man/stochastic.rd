\name{stochastic}
\Rdversion{1.1}
\alias{fsClassify}
\alias{fsClassify-methods}
\alias{fsClassify,RLBigData-method}
\alias{fsClassify,RecLinkData-method}
\alias{fsWeights}
\alias{fsWeights-methods}
\alias{fsWeights,RLBigData-method}
\alias{fsWeights,RecLinkData-method}

\title{
  Stochastic record linkage.
}
\description{
  Methods for stochastic record linkage following the framework
  of Fellegi and Sunter.
}

\usage{


\S4method{fsWeights}{RecLinkData}(rpairs, m = 0.95, u = rpairs$frequencies, cutoff = 1)
\S4method{fsWeights}{RLBigData}(rpairs, m=0.95, u=getFrequencies(rpairs),
    cutoff=1, withProgressBar = (sink.number()==0))
\S4method{fsClassify}{RecLinkData}(rpairs, ...)
\S4method{fsClassify}{RLBigData}(rpairs, threshold.upper, threshold.lower=threshold.upper, 
  m=0.95, u=getFrequencies(rpairs), withProgressBar = (sink.number()==0), cutoff=1)

}

\arguments{
  \item{rpairs}{The record pairs to be classified.}
  \item{threshold.upper}{A numeric value between 0 and 1.}
  \item{threshold.lower}{A numeric value between 0 and 1 lower than \code{threshold.upper}.}
  \item{m, u}{Numeric vectors. m- and u-probabilities of matching variables, see Details.}
  \item{withProgressBar}{Logical. Whether to display a progress bar.}
  \item{cutoff}{Numeric value. Threshold for converting string comparison values
    to binary values.}
  \item{...}{Arguments passed to emClassify.}
}

\details{

  These methods perform stochastic record linkage following the framework of
  Fellegi and Sunter (see reference).
  
  \code{fsWeights} calculates matching weights on an object based on the
  specified m- and u-probabilites. Each of \code{m} and \code{u} can be a
  numeric vector or a single number in the range \eqn{[0, 1]}.

  \code{fsClassify} performs classification based on the calculated weights.
  All record pairs with weights greater or
  equal \code{threshold.upper} are classified as links. Record pairs with
  weights smaller than \code{threshold.upper} and greater or equal
  \code{threshold.lower} are classified as possible links. All remaining
  records are classified as non-links.

  The \code{"RecLinkData"} method is a shortcut for \code{\link{emClassify}}.

  The \code{"RLBigData"} method checks if weights are
  present in the underlying database. If this is the case, classification
  is based on the existing weights. If not, weights are calculated on the fly
  during classification, but not stored. The latter behaviour might be preferable
  when a very large dataset is to be classified and disk space is limited.
  A progress bar is displayed only if
  weights are calculated on the fly and, by default, unless output is diverted by
  \code{\link{sink}} (e.g. in a Sweave script).
  
  For a general introduction to weight based record linkage, see the vignette
  "Weight-based deduplication".
}

\value{
  \code{fsWeights} returns a copy of the object with the calculated weights
  added. Note that \code{"\linkS4class{RLBigData}"} objects have some
  reference-style semantics, see \link{clone} for more information.

  For the \code{"\link{RecLinkData}"} method, \code{fsClassify} returns a S3 object
  of class \code{"\link{RecLinkResult}"} that represents a copy
  of \code{newdata} with element \code{rpairs$prediction}, which stores
  the classification result, as addendum.

  For the \code{"\linkS4class{RLBigData}"} method, \code{fsClassify} returns
  a S4 object of class \code{"\linkS4class{RLResult}"}.
}

\author{Andreas Borg, Murat Sariyar}

\seealso{
  \code{\link{epiWeights}}
}

\references{Ivan P. Fellegi, Alan B. Sunter: A Theory for Record Linkage,
  in: Journal of the American Statistical Association Vol. 64, No. 328
  (Dec., 1969), pp. 1183--1210.}

\examples{
# generate record pairs
data(RLdata500)
rpairs <- compare.dedup(RLdata500, blockfld=list(1,3,5,6,7), identity=identity.RLdata500)

# calculate weights
rpairs <- fsWeights(rpairs)

# classify and show results
summary(fsClassify(rpairs,0))
}
\keyword{classif}
