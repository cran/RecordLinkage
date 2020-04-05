\name{RecLinkResult-class}
\Rdversion{1.1}
\docType{class}
\alias{RecLinkResult-class}

\title{Class "RecLinkResult"}
\description{
  S4 wrapper for S3 class \code{"\link{RecLinkResult}"}.
}
\section{Objects from the Class}{Object of the S3 class are created by
  classification functions, such as \code{\link{classifySupv}} and 
  \code{\link{emClassify}}. The S4 class is virtual and exists solely 
  for internal usage in method signatures.
}

\section{Slots}{
  \describe{
    \item{\code{.S3Class}:}{Internal slot.}
  }
  See \code{"\link{RecLinkResult}"} for the structure of the S3 class.
}

\section{Extends}{
  Class \code{"\linkS4class{RecLinkData}"}, directly.
  Class \code{"\linkS4class{oldClass}"}, by class "RecLinkData", distance 2.
}

\section{Methods}{
  \describe{
    \item{\%append\%}{\code{signature(x = "RecLinkResult", y = "RecLinkResult")}}
    \item{getErrorMeasures}{\code{signature(object="RecLinkResult")}}
    \item{getTable}{\code{signature(object="RecLinkResult")}}
	 }
}

\author{
  Andreas Borg, Murat Sariyar
}


\seealso{
  \code{"\link{RecLinkResult}"} for the structure of the S3 class.
  \code{"\linkS4class{RLResult}"}, the equivalent data structure for
  big data sets.
}


\examples{
showClass("RecLinkResult")
}
\keyword{classes}
\keyword{classif}
