\name{makeBlockingPairs}
\alias{makeBlockingPairs}

\title{
  Create record pairs from blocks of ids.
}
\description{
  Create record pairs from blocks of ids. 
}
\usage{
makeBlockingPairs(id_vec)
}
\arguments{
  \item{id_vec}{
    A list of integer vectors.
  }
}
\details{
  Each element in \code{id_vec} holds the ids of a set of records which agree
  for a particular blocking variable (see \code{\link{compare.dedup}} for the
  blocking mechanism). For each block, all unordered pairs of the ids are 
  created and concatenated in the output.
}
\value{
  A matrix with two columns, each row holding the ids of one record pair.
}
\author{
  Andreas Borg
}
\note{
Internal function used by \code{\link{compare.dedup}}.
}

\keyword{internal}

