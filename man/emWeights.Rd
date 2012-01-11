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

  \S4method{emWeights}{RLBigData}(rpairs, cutoff = 0.95,
    verbose = TRUE, ...)
}
\arguments{
  \item{rpairs}{The record pairs for which to
    compute weights. See details.}
  \item{cutoff}{Either a numeric value in the range [0,1] or a vector with the same 
  length as the number of attributes in the data. Cutoff value for string comparator.}
  \item{verbose}{Logical. Whether to print progress messages.}
  \item{\dots}{Additional arguments passed to \code{\link{mygllm}}.}
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
  
  Some progress messages are printed to the message stream (see
  \code{\link{message}} if \code{verbose == TRUE}.
  This includes progress bars, but these are supressed if output is diverted by
  \code{\link{sink}} to avoid cluttering the output file.
}

\value{
  A copy of \code{rpairs} with the weights attached. See the class documentation
  (\code{"\link[=RecLinkData.object]{RecLinkData}"}, \code{"\linkS4class{RLBigDataDedup}"} and
  \code{"\linkS4class{RLBigDataLinkage}"}) on how weights are stored.
}

\section{Side effects}{
  The \code{"\linkS4class{RLBigData}"} method writes to a disk file containing
  a \code{ffvector} that contains the calculated weights.
  belonging to \code{object}
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
