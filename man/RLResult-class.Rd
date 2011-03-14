\name{RLResult-class}
\Rdversion{1.1}
\docType{class}
\alias{RLResult-class}

\title{Class "RLResult"}
\description{
  A class that represents the result of a record linkage procedure with
  big data sets.
}
\section{Objects from the Class}{
  Objects from this class are created by the classification functions in
  this package, e.g. \code{\link{classifySupv}}. Directly creating instances
  by calling \code{new} is neither necessary nor recommended.
}

\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"RLBigData"}. The data set
      which was classified.}
    \item{\code{links}:}{Object of class \code{"matrix"} with two columns,
      holding the ids of pairs which were classified as a link.}
    \item{\code{possibleLinks}:}{Object of class \code{"matrix"} with two
      columns, holding the ids of pairs which were classified as a
      possible link.}
    \item{\code{nPairs}:}{Object of class \code{"numeric"}. The total number of
    classified record pairs.}
  }
}

\section{Methods}{
  \describe{
    \item{clone}{\code{signature(object = "RLResult")}}
    \item{getDbFile}{\code{signature(object = "RLResult")}}
    \item{getErrorMeasures}{\code{signature(object = "RLResult")}}
    \item{getPairs}{\code{signature(object = "RLResult")}}
    \item{getTable}{\code{signature(object = "RLResult")}}
    \item{saveRLObject}{\code{signature(object = "RLResult")}}
         }
}

\author{
  Andreas Borg, Murat Sariyar
}
\note{
  The slot \code{data} uses a database to store data and create
  comparison patterns, thus assignment of a \code{"RLResult"} object to a
  different variable can lead to undesired results. Use \code{clone} to make
  a distinct copy. Similarly, the standard save mechanism does not work;
  \code{saveRLObject} and \code{loadRLObject} are provided to make objects
  persistent over different R sessions.
}

\seealso{
  \code{\link{classifySupv}}, \code{\link{emClassify}} and
  \code{\link{epiClassify}} create objects of this type.
}
\keyword{classes}
\keyword{classif}
