\name{phonetics}
\alias{phonetics}
\alias{soundex}

\title{Phonetic Code}
\description{
  Interface to phonetic coding functions.
}
\usage{
soundex(str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{A character vector or matrix.}
}
\details{
  \code{soundex} is a widespread algorithm for English names. This implementation
  can only handle common characters. It strips off non-alphabetical characters.
  
  The C code for \code{soundex} was taken from PostgreSQL 8.3.6.  
}

\value{
  A character vector or matrix with the same size and dimensions as \code{str},
  containing its phonetic encoding.
}


\references{see also \url{https://www.codedrome.com/the-soundex-algorithm-in-c/}
}
\author{Andreas Borg (R interface only)}

\seealso{\code{\link{jarowinkler}} and \code{\link{levenshteinSim}}
  for string comparison.}

\keyword{misc}
