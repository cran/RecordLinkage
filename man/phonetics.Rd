\encoding{latin1}

\name{phonetics}
\alias{phonetics}
\alias{pho_h}
\alias{soundex}

\title{Phonetic Code}
\description{
  Interface to phonetic coding functions.
}
\usage{
pho_h(str)
soundex(str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{A character vector or matrix. Factors are converted to character.}
}
\details{
  Translates its argument to a phonetic code. \code{pho_h}
  by \enc{Jörg}{Joerg} Michael (see references) is intended for German language 
  and normalizes umlauts and accent characters.
  \code{soundex} is a widespread algorithm for English names. This implementation
  can only handle common characters. Both algorithms strip off 
  non-alphabetical characters, with the exception that numbers are left
  unchanged by \code{pho_h}.
  
  The C code for \code{soundex} was taken from PostgreSQL 8.3.6.  
}

\value{
  A character vector or matrix with the same size and dimensions as \code{str},
  containing its phonetic encoding.
}


\references{\enc{Jörg}{Joerg} Michael, \enc{Doppelgänger}{Doppelgaenger} gesucht
  -- Ein Programm \enc{für}{fuer} kontextsensitive phonetische Textumwandlung,
  in: c't 1999, No. 25, pp. 252--261. 
  The Source code is published (under GPL) at 
  \url{http://www.heise.de/ct/ftp/99/25/252/}.
}
\author{Andreas Borg (R interface only)}

\seealso{\code{\link{jarowinkler}} and \code{\link{levenshteinSim}}
  for string comparison.}


\keyword{misc}
