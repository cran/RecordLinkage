\name{RLBigDataDedup-class}
\Rdversion{1.1}
\docType{class}
\alias{RLBigDataDedup-class}

\title{Class "RLBigDataDedup"}
\description{
  Represents a record linkage setup where a single dataset is to
  be deduplicated.
}
\section{Objects from the Class}{
Objects should be created using the constructor function
\code{\link{RLBigDataDedup}}, which does some essential error checking,
  conversion and initialization.
}
\section{Slots}{
  See also \code{"\linkS4class{RLBigData}"} for inherited slots.
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} Data set. }
    \item{\code{identity}:}{Object of class \code{"factor"} True ID of records in \code{data} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{RLBigData}"}, directly.
}

\section{Methods}{
  \describe{
    \item{getColumnNames}{\code{signature(object = "RLBigDataDedup")}}
    \item{getExpectedSize}{\code{signature(object = "RLBigDataDedup")}}
 	 }
	 See also \link{RLBigData-class} for inherited methods.
}
\author{
  Andreas Borg
}


\seealso{
  \code{\link{RLBigDataDedup}}, \link{RLBigData-class}
}
\examples{
showClass("RLBigDataDedup")
}
\keyword{classes}
