\name{getPairs}
\alias{getPairs}

\title{Extract Record Pairs}
\description{
  Extracts record pairs from data and result objects.
}
\usage{
getPairs(rpairs, max.weight = Inf, min.weight = -Inf, 
         single.rows = FALSE, show = "all", sort = !is.null(rpairs$Wdata))
}

\arguments{
  \item{rpairs}{A \code{\link{RecLinkResult}} or \code{\link{RecLinkData}}
    object.}
  \item{max.weight, min.weight}{Real numbers. Upper and lower threshold.}
  \item{single.rows}{Logical. Wether to print record pairs in one row instead 
    of two consecutive rows.}
  \item{show}{Selects which records to show, one of \code{"links"}, 
    \code{"nonlinks"}, \code{"possible"}, \code{"all"}.}
  \item{sort}{Logical. Whether to sort descending by weight.}
}

\details{
  This function extracts record pairs from a \code{\link{RecLinkData}} 
  or \code{\link{RecLinkResult}} object for further processing such as
  a review of possible links. Arguments \code{max.weight}, \code{min.weight}
  and \code{show} control which records to include in the output. If weights
  are stored in \code{rpairs$Wdata}, all records with
  \code{rpairs$Wdata < max.weight & rpairs$Wdata >= min.weight}
  are returned.
  
  The selection can be customized by \code{show} to include all data pairs,
  only links, only non-links or only possible links.
  
  If \code{single.rows} is not \code{TRUE}, pairs are output on two consecutive
  lines in order to enhance a manual review. All data are converted to character, which 
  can lead to a loss of precision for numeric values. 
  Therefore, two row-format should be used  for printing only.
}
\value{
  A data frame. If \code{single.rows} is \code{TRUE}, each row holds (in this
  order) the weight of the data pair (possibly \code{NA}), id and data fields of the 
  first record and
  id and data fields of the second record.

  If \code{single.rows} is not \code{TRUE}, each odd row contains the weight
  followed by id and data fields of the first record, each even row a blank 
  field followed by id and data fields of the second record.
}
\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{RecLinkResult}}, \code{\link{RecLinkData}}}

\keyword{classif}
