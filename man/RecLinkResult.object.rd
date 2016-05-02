\name{RecLinkResult.object}
\alias{RecLinkResult}
\title{
  Record Linkage Result Object
}
\description{
S3 class representing information about the classification result of a
  Record Linkage procedure and the data from which this result is derived.
}

\value{
  A list with all mandatory and optional elements of
  class \code{"\link{RecLinkData}"} plus the following:

  \describe{
    \item{\code{prediction}:}{Object of class \code{"factor"}
    Classification of each record, corresponding to the record pairs stored
    in component \code{pairs}. Levels are:
      \describe{
        \item{\code{"N"}}{for non-links.}
        \item{\code{"P"}}{for possible links,}
        \item{\code{"L"}}{for links,}
      }}
  }
}




\seealso{
  \code{"\linkS4class{RecLinkResult}"} for the S4 representation.
  \code{"\link{RecLinkData}"} for the superclass.
  \code{"\linkS4class{RLResult}"}, the equivalent data structure for
  big data sets.
}

\author{Andreas Borg, Murat Sariyar}
\keyword{classif}
