\name{RecLinkData-class}
\Rdversion{1.1}
\docType{class}
\alias{RecLinkData-class}

\title{Class "RecLinkData"}
\description{
  S4 wrapper for S3 class \code{"\link{RecLinkData}"}.
}

\section{Objects from the Class}{Objects of the S3 class are created by
  the comparison functions \code{\link[=compare.dedup]{compare.*}}.
  The S4 class is virtual and exists solely for internal usage in method signatures.
}

\section{Slots}{
  \describe{
    \item{\code{.S3Class}:}{Internal slot.}
  }
  See \code{"\link{RecLinkData}"} for the structure of the S3 class.
}

\section{Extends}{
Class \code{"\linkS4class{oldClass}"}, directly.
}
\section{Methods}{
  Use \code{getMethods(classes = "RecLinkData")} to list the methods defined for
  this class.
}

\author{
Andreas Borg, Murat Sariyar
}


\seealso{
  \code{"\link{RecLinkData}"} for the structure of the S3 class.
  \code{\link{compare.dedup}}, which creates objects of this class.
  \code{"\linkS4class{RLBigData}"}, an alternative data structure
  suitable for big data sets.
}
\examples{
showClass("RecLinkData")
}
\keyword{classes}
\keyword{classif}
