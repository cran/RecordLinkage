\name{getErrorMeasures-methods}
\docType{methods}
\alias{getErrorMeasures}
\alias{getErrorMeasures-methods}
\alias{getErrorMeasures,RecLinkResult-method}
\alias{getErrorMeasures,RLResult-method}
\alias{errorMeasures}

\title{Calculate Error Measures}
\description{
  Computes various error measures for the classification of a data set.
}

\details{
  
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
    \item{ppv}{Positive predictive value:}\eqn{\frac{\mathit{TP}}{\mathit{TP}+\mathit{FP}}}{TP/(TP+FP)}
    \item{npv}{Nositive predictive value:}\eqn{\frac{\mathit{TN}}{\mathit{TN}+\mathit{FN}}}{TN/(TN+FN)}
  }
}

\value{
  A list with components \code{alpha}, \code{beta}, \code{accuracy}, 
  \code{precision}, \code{sensitivity}, \code{specificity}, \code{ppv} and
  \code{npv}, each a number in the range \eqn{[0,1]}.
}

\note{Record pairs with unknown true matching status (e.g. due to missing
  values in the argument \code{identity} to \code{\link{RLBigDataDedup}})
  and possible links are not counted, which can distort the values returned
  by this function.
}

\section{Methods}{
  \describe{
  
    \item{\code{signature(object = "RecLinkResult")}}{
    Method for S3 result objects of class \code{"RecLinkResult"} }
    
    \item{\code{signature(object = "RLResult")}}{
      Method for S4 objects of class \code{\link[=RecLinkResult-class]{"RLResult"}}, 
      from classification of big data objects (see \code{"\linkS4class{RLBigData}"},
      \code{"\linkS4class{RLBigDataDedup}"}, \code{"\linkS4class{RLBigDataLinkage}"})
    }
  }
  A wrapper function \code{errorMeasures(result)} exists for compatibility with package version
  0.2.
}

\author{
  Murat Sariyar, Andreas Borg
}

\keyword{methods}
\keyword{classif}
