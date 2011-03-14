\name{emWeights}
\alias{emWeights}
\alias{emWeights-methods}
\alias{emWeights,RecLinkData-method}
\alias{emWeights,RLBigData-method}

\title{Calculate weights}
\description{
  Calculates weights for Record Linkage based on an EM algorithm.
}
\usage{
  emWeights(rpairs, cutoff = 0.95, ...)

  \S4method{emWeights}{RecLinkData}(rpairs, cutoff = 0.95, ...)

  \S4method{emWeights}{RLBigData}(rpairs, cutoff = 0.95, store.weights = TRUE, ...)
}
\arguments{
  \item{rpairs}{The record pairs for which to
    compute weights. See details.}
  \item{cutoff}{Either a numeric value in the range [0,1] or a vector with the same 
  length as the number of attributes in the data. Cutoff value for string comparator.}
  \item{store.weights}{Logical value. Wether to store individual weights in the
    database}
  \item{\dots}{Additional arguments for \code{\link{mygllm}}.}
}
\details{
  Since package version 0.3, this is a generic functions with methods for
  S3 objects of class \code{"\link[=RecLinkData.object]{RecLinkData}"} as well as S4 objects
  of classes \code{"\linkS4class{RLBigDataDedup}"} and
  \code{"\linkS4class{RLBigDataLinkage}"}.
  
  The weight of a record pair is calculated by \eqn{\log_{2}\frac{M}{U}}{
  log(M/U,base=2)}, where \eqn{M} and \eqn{U} are estimated m- and u-probabilities
  for the present comparison pattern. If a string comparator is used, weights
  are first calculated based on a binary table where all comparison 
  values greater or equal \code{cutoff} are set to one, all other to zero.
  The resulting weight is adjusted by adding for every pair
  \eqn{\log_{2}\left(\prod_{j:s^{i}_{j}\geq \textit{cutoff }}s^{i}_{j}\right)}{
  log(prod(s[i,s[i,]>=cutoff_j]), base = 2)}, where
  \eqn{s^{i}_{j}}{s[i,j]} is the value of the string metric for attribute j in 
  data pair i.
  
  The appropriate value of \code{cutoff} depends on the choice of string
  comparator. The default is adjusted to \code{\link{jarowinkler}},
  a lower value (e.g. 0.7) is recommended for \code{\link{levenshteinSim}}.
  
  Estimation of \eqn{M} and \eqn{U} is done by an EM algorithm, implemented by
  \code{\link{mygllm}}. For every comparison
  pattern, the estimated numbers of matches and non-matches are used to compute
  the corresponding probabilities. Estimations based on the average 
  frequencies of values and given error rates are taken as initial values.
  In our experience, this increases stability and performance of the
  EM algorithm.
  
  The \code{"\linkS4class{RLBigData}"} method writes the individual weight
  for every record pairs into the database if called with
  \code{store.weights=TRUE}. This speeds up subsequent calls of the
  classification function \code{\link{emClassify}} and is in general recommended
  if several classification calls are to be made (e.g. for testing different
  thresholds). However, if a very large number of record pairs is processed,
  saving individual weights can lead to excessive disk usage; in this case
  \code{store.weights = FALSE} may be a better choice. Subsequent calls to
  \code{\link{emClassify}} will then calculate individual weights on the fly
  during classification without saving them.
}

\value{
  A copy of \code{rpairs} with the weights attached. See the class documentation
  (\code{"\link[=RecLinkData.object]{RecLinkData}"}, \code{"\linkS4class{RLBigDataDedup}"} and
  \code{"\linkS4class{RLBigDataLinkage}"}) on how weights are stored.
}

\section{Side effects}{
  The \code{"\linkS4class{RLBigData}"} method writes weights to the database
  belonging to \code{object}
}

\note{
  The \code{"\linkS4class{RLBigData}"} method changes the database to which
  \code{rpairs} points. If you want the object unchanged, make a copy via
  \code{\link{clone}} first.
}

\references{William E. Winkler: Using the EM Algorithm for Weight Computation 
  in the Fellegi-Sunter Model of Record Linkage, in: Proceedings of the Section 
  on Survey Research Methods, American Statistical Association 1988, 
  pp. 667--671.}
\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{emClassify}} for classification of weighted pairs.
  \code{\link{epiWeights}} for a different approach for weight calculation.
}

\keyword{classif}
