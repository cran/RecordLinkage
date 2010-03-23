\name{emClassify}
\alias{emClassify}

\title{Weight-based Classification of Data Pairs}
\description{
  Classifies data pairs to which weights were assigned by \code{\link{emWeights}}
  based on user-defined thresholds or estimated error rates.
}
\usage{
emClassify(rpairs, threshold.upper = Inf, 
  threshold.lower = threshold.upper, my = Inf, ny = Inf)
}

\arguments{
  \item{rpairs}{\code{\link{RecLinkData}} object with weight information.}
  \item{my}{A probability. Error bound for false positives.}
  \item{ny}{A probability. Error bound for false negatives.}
  \item{threshold.upper}{A numeric value. Threshold for links.}
  \item{threshold.lower}{A numeric value. Threshold for possible links.}
}
\details{
  Two general approaches are implemented for classification. The classical procedure
  by Fellegi and Sunter (see references) minimizes the number of
  possible links with given error levels for false links (\code{my}) and
  false non-links (\code{ny}).
  
  The second approach requires thresholds for links and possible links to be set
  by the user. A pair with weight \eqn{w} is classified as a link if 
  \eqn{w\geq \textit{threshold.upper}}{w>=\textit{threshold.upper}}, as a possible link if 
  \eqn{\textit{threshold.upper}\geq w\geq \textit{threshold.lower}}{threshold.upper>=w>=
  treshold.lower} and as a non-link if \eqn{w<\textit{threshold.lower}}{w<threshold.lower}.
  
  If \code{threshold.upper} or \code{threshold.lower} is given, the 
  threshold-based approach is used, otherwise, if one of the error bounds is
  given, the Fellegi-Sunter model. If only \code{my} is supplied, links are
  chosen to meet the error bound and all other pairs are classified as non-links
  (the equivalent case holds if only \code{ny} is specified). If no further arguments
  than \code{rpairs} are given, a single threshold of 0 is used.
  
%- alter Text:  
%-  , which 
%-  a classification in links and non-links which
%-  minimizes the overall error is performed based on the estimated probabilities.
  
}

\note{The quality of classification of the Fellegi-Sunter method 
  relies strongly on a good 
  estimation of m- and u-probabilities. The results should be evaluated 
  critically.
}

\value{
  Returns a \code{RecLinkResult} object containing all fields of \code{rpairs}
  and a factor object \code{prediction} which contains predictions and corresponds
  to \code{rpairs$pairs}. \code{"L"} represents a link, \code{"N"} a non-link and
  \code{"P"} a possible link.
}

\references{Ivan P. Fellegi, Alan B. Sunter: A Theory for Record Linkage,
  in: Journal of the American Statistical Association Vol. 64, No. 328 
  (Dec., 1969), pp. 1183--1210.}

\author{Andreas Borg}

\seealso{\code{\link{getPairs}} to produce output from which thresholds can
  be determined conveniently.}
  
\keyword{classif}

