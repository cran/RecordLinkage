\name{RLBigDataLinkage-class}
\Rdversion{1.1}
\docType{class}
\alias{RLBigDataLinkage-class}

\title{Class "RLBigDataLinkage"}
\description{
  Represents a record linkage setup with two datasets which are to 
  be linked together.
}
\section{Objects from the Class}{
Objects should be created using the constructor function 
\code{\link{RLBigDataLinkage}}, which does some essential error checking, 
  conversion and initialization.

}
\section{Slots}{
  See also \code{"\linkS4class{RLBigData}"} for inherited slots.
  \describe{
    \item{\code{data1}:}{Object of class \code{"data.frame"} First data set. }
    \item{\code{data2}:}{Object of class \code{"data.frame"} Second data set. }
    \item{\code{identity1}:}{Object of class \code{"factor"} True ID of records in \code{data1} }
    \item{\code{identity2}:}{Object of class \code{"factor"} True ID of records in \code{data2}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{RLBigData}"}, directly.
}

\section{Methods}{
  \describe{
    \item{getColumnNames}{\code{signature(object = "RLBigDataLinkage")}}
    \item{getExpectedSize}{\code{signature(object = "RLBigDataLinkage")}}
  }
  See also \link{RLBigData-class} for inherited methods.
}

\author{
  Andreas Borg
}

\seealso{
  \code{"\linkS4class{RLBigData}"}, \code{\link{RLBigDataLinkage}}
}
\examples{
showClass("RLBigDataLinkage")
}
\keyword{classes}
