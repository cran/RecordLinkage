\name{deleteNULLs}
\alias{deleteNULLs}

\title{Remove NULL Values}
\description{
  Removes all \code{NULL} elements from a list or vector.
}
\usage{
deleteNULLs(x)
}

\arguments{
  \item{x}{A vector or list}
}

\value{
  A copy of \code{x} with \code{NULL} values removed.
}

\references{Taken from a posting by Jim Holtman on the R-help mailing list,
  \url{https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html}}


\note{This function is internally used for simple lists and vectors. 
  The behaviour for nested lists and vectors embedded in lists is not 
  thoroughly tested.
}

\keyword{misc}
