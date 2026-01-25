\name{resample}
\alias{resample}

\title{Safe Sampling}
\description{                                                          
  Performs sampling without replacement while avoiding undesired behaviour if
  \code{x} has length 1.
  See documentation of \code{\link{sample}}.
}

\usage{
resample(x, size, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector from which to sample.}
  \item{size}{A non-negative number giving the size of the sample.}
  \item{\dots}{Further arguments to \code{\link{sample}}.}
}

\keyword{misc}
