\name{mrl}
\Rdversion{1.1}
\alias{mrl}
\alias{plotMRL}

\title{
  Mean Residual Life Plot
}

\description{
  Create mean residual life statistics for classification based on extreme
  value theory.
}

\usage{
mrl(data, umin = min(data), umax = max(data) - 0.1, 
  nint = round(max(data) - min(data)) * 20)

plotMRL(rpairs, l = .computeMRL(sort(as.ram((rpairs$Wdata)))))
}

\arguments{
  \item{data}{
    A numerical vector, representing weights in the context of Record Linkage.
}
  \item{umin, umax}{
    The minimum and maximum thresholds at which the mean residual life function is calculated.
}
  \item{nint}{
    The number of points at which the mean residual life function is calculated.
  }
  \item{rpairs}{
    A \code{\link{RecLinkData}} object with assigned weights.
  }
  \item{l}{
    A list with components \code{x} and \code{y} representing MRL statistics
    as returned by \code{mrl}.
  }
}

\details{
  While \code{mrl} only returnes MRL statistics as a list, \code{plotMRL}
  produces a plot thereof. These functions are used internally for threshold
  estimation based on Extreme Value Theory, see 
  \code{\link{getParetoThreshold}}.
  
  \code{mrl} is a simplified version of \code{mrl.plot} in package \code{ismev}.
}

\value{
\code{mrl} returnes a list with 
  \item{x}{x-coordinates of MRL statistics}
  \item{y}{y-coordinates of MRL statistics}
}


\seealso{
  \code{\link{getParetoThreshold}}
}

\keyword{internal}

