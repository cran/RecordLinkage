\name{getPairs}
\alias{getPairs}
\alias{getPairs,RecLinkData-method}
\alias{getPairs,RecLinkResult-method}
\alias{getPairs-methods}
\alias{getPairs,RLResult-method}
\alias{getPairs,RLBigData-method}
\alias{getFalsePos}
\alias{getFalseNeg}
\alias{getFalse}

\title{Extract Record Pairs}
\description{
  Extracts record pairs from data and result objects.
}
\usage{
\S4method{getPairs}{RecLinkData}(object, max.weight = Inf, min.weight = -Inf,
         single.rows = FALSE, show = "all", sort = !is.null(object$Wdata))

\S4method{getPairs}{RLBigData}(object, max.weight = Inf, min.weight = -Inf,
    filter.match = c("match", "unknown", "nonmatch"),
    withWeight = hasWeights(object), withMatch = TRUE, single.rows = FALSE,
    sort = withWeight)

\S4method{getPairs}{RLResult}(object, filter.match = c("match", "unknown", "nonmatch"),
    filter.link = c("nonlink", "possible", "link"), max.weight = Inf, min.weight = -Inf,
    withMatch = TRUE, withClass = TRUE, withWeight = hasWeights(object@data),
    single.rows = FALSE, sort = withWeight)

getFalsePos(object, single.rows = FALSE)
getFalseNeg(object, single.rows = FALSE)
getFalse(object, single.rows = FALSE)

}


\arguments{
  \item{object}{The data or result object from which to extract record pairs.}
  \item{max.weight, min.weight}{Real numbers. Upper and lower weight threshold.}
  \item{filter.match}{Character vector, a nonempty subset of \code{c("match", "nonmatch", "unkown")}
    denoting which pairs to allow in the output.}
  \item{filter.link}{Character vector, a nonempty subset of \code{c("link", "nonlink", "unkown")}
    denoting which pairs to allow in the output.}
  \item{withWeight}{Logical. Whether to include linkage weights in the output.}
  \item{withMatch}{Logical. Whether to include matching status in the output.}
  \item{withClass}{Logical. Whether to include classification result in the output.}
  \item{single.rows}{Logical. Wether to print record pairs in one row instead
    of two consecutive rows.}
  \item{show}{Character. Selects which records to show, one of \code{"links"},
    \code{"nonlinks"}, \code{"possible"}, \code{"all"}.}
  \item{sort}{Logical. Whether to sort descending by weight.}
}

\details{
  These methods extract record pairs from \code{"\link{RecLinkData}"},
  or \code{"\link{RecLinkResult}"}, \code{"\linkS4class{RLBigData}"} and
  \code{"\linkS4class{RLResult}"} objects. Possible applications are retreiving
  a linkage result for further processing, conducting a manual review in order
  to determine classification thresholds or inspecting misclassified pairs.
  
  The various arguments can be grouped by the following purposes:
  \enumerate{
    \item{Controlling which record pairs are included in the output:
      \code{min.weight} and \code{max.weight}, \code{filter.match},
      \code{filter.link}, \code{show}.}
    \item{Controlling which information is shown: \code{withWeight}, \code{withMatch},
      \code{withClass}}
    \item{Cotrolling the overall structure of the result: \code{sort},
      \code{single.rows}.}
  }

  The weight limits are inclusive, i.e. a record pair with weight \code{w}
  is included only if \code{w >= min.weight && w <= max.weight}.

  If \code{single.rows} is not \code{TRUE}, pairs are output on two consecutive
  lines in a more readable format. All data are converted to character, which
  can lead to a loss of precision for numeric values.
  Therefore, this format should be used  for printing only.
  
  \code{getFalsePos}, \code{getFalseNeg} and \code{getFalse} are shortcuts
  (currently for objects of class \code{"\linkS4class{RLResult}"} only)
  to retreive false positives (links that are non-matches in fact),
  false negatives (non-links that are matches in fact) or all falsly classified
  pairs, respectively.
}
\value{
  A data frame. If \code{single.rows} is \code{TRUE}, each row holds (in this
  order) id and data fields of the
  first record, id and data fields of the second record and possibly matching
  status, classification result and/or weight.

  If \code{single.rows} is not \code{TRUE}, the result holds for each resulting
  record pair consecutive rows of the following format:
  \enumerate{
    \item{ID and data fields of the first record followed by as many empty
      fields to match the length of the following line.}
    \item{ID and data fields of the second record, possibly followed by
      matching status, classification result and/or weight.}
    \item{A blank line to separate record pairs.}
  }
}

\note{

      When non-matches are included in the output and blocking is permissive,
      the result object can be very large, possibly leading to memory problems.
}

\author{Andreas Borg, Murat Sariyar}

\examples{
data(RLdata500)

# create record pairs and calculate epilink weights
rpairs <- RLBigDataDedup(RLdata500, identity = identity.RLdata500,
  blockfld=list(1,3,5,6,7))
rpairs <- epiWeights(rpairs)

# show all record pairs with weights between 0.5 and 0.6
getPairs(rpairs, min.weight=0.5, max.weight=0.6)

# show only matches with weight <= 0.5
getPairs(rpairs, max.weight=0.5, filter.match="match")

# classify with one threshold
result <- epiClassify(rpairs, 0.5)

# show all links, do not show classification in the output
getPairs(result, filter.link="link", withClass = FALSE)

# see wrongly classified pairs
getFalsePos(result)
getFalseNeg(result)
}
\keyword{classif}
