\name{editMatch}
\Rdversion{1.1}
\alias{editMatch}
\alias{editMatch-methods}
\alias{editMatch,RecLinkData-method}
\alias{editMatch,RLBigData-method}


\title{
  Edit Matching Status
}
\description{
  Allows editing the matching status of record pairs.
}
\usage{
editMatch(rpairs)
}

\arguments{
  \item{rpairs}{
    A \code{"\link{RecLinkData}"} or \code{"\linkS4class{RLBigData}"} object.
      The record pairs to edit.
  }
}

\details{
  This function pops up an editor (via \code{\link{edit}}) where each record
  pair in \code{rpairs} is printed in two consecutive lines, pairs separated by blank
  lines. The matching status is printed and can be edited in the last column
  following the first respective record. A match is denoted by 1, a non-match
  by 0. \code{NA}s are possible to mark pairs with unknown status. Changes in
  other fields are ignored.
  
  Manual editing of the matching status is useful for clerical review in 
  general and in particular to label training sets. In conjunction with
  \code{\link{getMinimalTrain}}, good results can be obtained with a manageable
  effort of manual review.
}
\value{
  A copy of \code{rpairs} with edited matching status.
}


\author{
  Andreas Borg
}

\seealso{
  \code{\link{getMinimalTrain}}
}

\keyword{classif}