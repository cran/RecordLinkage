\name{epiWeights}
\Rdversion{1.1}
\alias{epiWeights}
\alias{epiWeights-methods}
\alias{epiWeights,RecLinkData-method}
\alias{epiWeights,RLBigData-method}

\title{
Calculate EpiLink weights
}
\description{
  Calculates weights for record pairs based on the EpiLink approach
  (see references).
}
\usage{
  epiWeights(rpairs, e = 0.01, f, ...)

  \S4method{epiWeights}{RecLinkData}(rpairs, e = 0.01, f = rpairs$frequencies)

  \S4method{epiWeights}{RLBigData}(rpairs, e = 0.01, f = getFrequencies(rpairs),
    withProgressBar = (sink.number()==0))
}

\arguments{
  \item{rpairs}{The record pairs for which to
    compute weights. See details.}
  \item{e}{
    Numeric vector. Estimated error rate(s).
  }
  \item{f}{
    Numeric vector. Average frequency of attribute values.
  }
  \item{withProgressBar}{Whether to display a progress bar}
  \item{...}{Placeholder for method-specific arguments.}
}

\details{
  This function calculates weights for record pairs based on the approach
  used by Contiero et al. in the EpiLink record linkage software (see references).

  Since package version 0.3, this is a generic function with methods for
  S3 objects of class \code{"\link[=RecLinkData.object]{RecLinkData}"}
  as well as S4 objects of classes \code{"\linkS4class{RLBigDataDedup}"} and
  \code{"\linkS4class{RLBigDataLinkage}"}.

  The weight for a record pair \eqn{(x^{1},x^{2})}{(x1,x2)} is computed by
  the formula 
  \deqn{\frac{\sum_{i}w_{i}s(x^{1}_{i},x^{2}_{i})}{\sum_{i}w_{i}}}{sum_i (w_1 * s(x1_i, x2_i)) / sum_i w_i}
  where \eqn{s(x^{1}_{i},x^{2}_{i})}{s(x1_i, x2_i)} is the value of a string comparison of
  records \eqn{x^{1}}{x1} and \eqn{x^{2}}{x2} in the i-th field and 
  \eqn{w_{i}}{w_i} is a weighting factor computed by 
  \deqn{w_{i}=\log_{2}(1-e_{i})/f_{i}}{w_i = log_2 (1-e_i) / f_i}
   where \eqn{f_{i}}{f_i} denotes the
  average frequency of values and \eqn{e_{i}}{e_i} the estimated error rate
  for field \eqn{i}. 
  
  String comparison values are taken from the record pairs as they were
  generated with \code{\link{compare.dedup}} or \code{\link{compare.linkage}}.
  The use of binary patterns is possible, but in general yields poor results.
  
  The average frequency of values is by default taken from the object
  \code{rpairs}. Both frequency and error rate \code{e} can be set to a single 
  value, which will be recycled, or to a vector with distinct error rates for 
  every field. 
  
  The error rate(s) and frequencie(s) must satisfy 
  \eqn{e_{i}\leq{}1-f_{i}}{e[i] <= 1-f[i]} for all \eqn{i}, otherwise
  the functions fails. Also, some other rare combinations can result in weights
  with illegal values (NaN, less than 0 or greater than 1). In this case a
  warning is issued.  
  
  By default, the \code{"\linkS4class{RLBigDataDedup}"} method displays a
  progress bar unless output is diverted by \code{sink}, e.g. when processing
  a Sweave file.
}

\value{
  A copy of \code{rpairs} with the weights attached. See the class documentation
  (\code{"\link[=RecLinkData.object]{RecLinkData}"}, \code{"\linkS4class{RLBigDataDedup}"} and
  \code{"\linkS4class{RLBigDataLinkage}"}) on how weights are stored.
  
  For the \code{"RLBigData"} method, the returned object is only a shallow
  copy in the sense that it links to the same database file as \code{rpairs}
}

\section{Side effects}{
  The \code{"RLBigData"} method writes a table with weights in the database
  file of \code{rpairs}, which means that changes apply to the provided object
  (similar to pass-by-reference style). If the existing state of \code{rpairs}
  is to be preserved, a copy should be made using \code{\link{clone}} before
  applying this function.
}

\note{The \code{"RLBigData"} method writes record IDs and weight for every
  record pair to the database file, potentially resulting to large files.
  If disk space is an issue, \code{\link{epiClassify}} can be called directy;
  it will then calculate weights during classification without storing them
  in the database. Furthermore, using \code{\link{epiClassify}} directly
  is faster for a single classification run while precalculation by
  \code{epiWeights} is preferable if several runs of \code{\link{epiClassify}}
  (e.g. for testing different thresholds) are to be made.
}

\references{
P. Contiero et al., The EpiLink record linkage software, in: Methods of 
Information in Medicine 2005, 44 (1), 66--71.
}
\author{
Andreas Borg, Murat Sariyar
}

\seealso{
  \code{\link{epiClassify}} for classification based on EpiLink weights.
  \code{\link{emWeights}} for a different approach for weight calculation.
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
