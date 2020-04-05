\name{errorMeasures}
\Rdversion{1.1}
\alias{errorMeasures}

\title{
  Calculate Error Measures
}
\description{
  Computes various error measures for the classification of a data set.
}
\usage{
errorMeasures(result)
}

\arguments{
  \item{result}{
    A \code{\link{RecLinkResult}} object.
  }
}

\details{
  The true matching status must be known for all record pairs in \code{result},
  i. e. there must be no other values than 0 or 1 in 
  \code{result$pairs$is_match}.
  
  Let \eqn{\mathit{TP}}{TP} be the number of correctly classified matches 
  (true positives), \eqn{\mathit{TN}}{TN} the number of correctly classified 
  non-matches (true negatives), \eqn{\mathit{FP}}{FP} and \eqn{\mathit{FN}}{FN} 
  the number of misclassified non-matches and matches
  (false positives and false negatives). The calculated error measures are:
  \describe{
    \item{alpha error}{\eqn{\frac{\mathit{FN}}{\mathit{TP}+\mathit{FN}}}{FN/(TP+FN)}}
    \item{beta error}{\eqn{\frac{\mathit{FP}}{\mathit{TN}+\mathit{FP}}}{FP/(TN+FP)}}
    \item{accuracy}{\eqn{\frac{\mathit{TP}+\mathit{TN}}{\mathit{TP}+\mathit{TN}+\mathit{FP}+\mathit{FN}}}{(TP+TN)/(TP+TN+FP+FN)}}
    \item{precision}{\eqn{\frac{\mathit{TP}}{\mathit{TP}+\mathit{FP}}}{TP/(TP+FP)}}
    \item{sensitivity}{\eqn{\frac{\mathit{TP}}{\mathit{TP}+\mathit{FN}}}{TP/(TP+FN)}}
    \item{specificity}{\eqn{\frac{\mathit{TN}}{\mathit{TN}+\mathit{FP}}}{TN/(TN+FP)}}
  }
}
\value{
  A list with components \code{alpha}, \code{beta}, \code{accuracy}, 
  \code{precision}, \code{sensitivity} and \code{specificity}, each a number in
  the range \eqn{[0,1]}.
}

\author{
  Murat Sariyar, Andreas Borg
}

\keyword{classif}

