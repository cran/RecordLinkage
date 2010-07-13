\name{RecLinkResult.object}
\alias{RecLinkResult}
\title{
  Record Linkage Result Object 
}
\description{
  An object representing information about the classification result of a
  Record Linkage procedure and the data from which this result is derived.
}

\value{
  \item{data, pairs, frequencies}{
    Inherited from \code{\link{RecLinkData}}.
  }

  
  \item{prediction}{Factor object indicating the classification of each record
    pair in \code{valid}. Levels are:
    \describe{
      \item{\code{"L"}}{for links,}
      \item{\code{"P"}}{for possible links}
      \item{\code{"N"}}{for non-links}
    }
  }
    
}

\author{Andreas Borg, Murat Sariyar}

\seealso{
\code{\link{emClassify}}.
\code{\link{RecLinkData}}.
}

\keyword{classif}
