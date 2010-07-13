\name{epiWeights}
\Rdversion{1.1}
\alias{epiWeights}

\title{
Calculate EpiLink weights
}
\description{
  Calculates weights for record pairs based on the EpiLink approach
  (see references).
}
\usage{
epiWeights(rpairs, e = 0.01, f = rpairs$frequencies)
}

\arguments{
  \item{rpairs}{\code{\link{RecLinkData}} object. Record pairs for which
    weights are to be calculated.}
  \item{e}{
    Numeric vector. Estimated error rate(s).
}
  \item{f}{
    Numeric vector. Average frequency of attribute values.
}
}

\details{
  This function calculates weights for record pairs based on the approach
  used by Contiero et al. in the EpiLink record linkage software (see references).
  
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
}
\value{
  A copy of \code{rpairs} with the calculated weights stored in component
  \code{rpairs$Wdata}.
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
