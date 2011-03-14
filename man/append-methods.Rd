\name{\%append\%-methods}
\docType{methods}
\alias{\%append\%-methods}
\alias{\%append\%}
\alias{\%append\%,RecLinkData,RecLinkData-method}
\alias{\%append\%,RecLinkResult,RecLinkResult-method}

\title{
  Concatenate comparison patterns or classification results
}
\description{
  Combines two object of class  \code{"\link{RecLinkData}"}
  or \code{"\link{RecLinkResult}"} by concatenating comparison patterns
  and, if available, weights and classification results.
}

\usage{

x \%append\% y

\S4method{\%append\%}{RecLinkData,RecLinkData}(x, y)

\S4method{\%append\%}{RecLinkResult,RecLinkResult}(x, y)
}

\arguments{
  \item{x, y}{The objects to combine.}
}


\value{
  An object with class corresponding to the input objects which represents
  the concatenation of \code{x} and \code{y}. Its
  component \code{pairs} is \code{rbind(x$pairs, y$pairs)}. If both \code{x}
  and \code{y} have weights stored in component \code{Wdata}, the result
  gets \code{c(x$Wdata, y$Wdata)} as component \code{Wdata}. For the
  \code{"\link{RecLinkResult}"} method, the result also includes the
  concatenation of the predicted classes in \code{x} and \code{y} as
  component \code{prediction}.
}

\note{
  The methods perform only a minimum of integrity checks, so the user has to
  make sure that the underlying data, the formats of comparison patterns
  (e.g. excluded columns) and the type of weights (method and parameters of
  weight calculation) match.
}
\author{Andreas Borg, Murat Sariyar}

\examples{
data(RLdata500)
rpairs1=compare.dedup(RLdata500, blockfld=1, identity = identity.RLdata500)
rpairs2=compare.dedup(RLdata500, blockfld=3, identity = identity.RLdata500)

summary(rpairs1)
summary(rpairs2)
summary(rpairs1 \%append\% rpairs2)
}

\keyword{methods}
