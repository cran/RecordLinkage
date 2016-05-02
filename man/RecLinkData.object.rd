\name{RecLinkData.object}
\alias{RecLinkData.object}
\alias{RecLinkData}
\title{
  Record Linkage Data Object 
}
\description{
  S3 class representing information about record pairs for Record
  Linkage, as returned by functions \code{\link{compare.dedup}} and
  \code{\link{compare.linkage}}.

}

\value{
  A list with at least the following elements:
  \describe{
    \item{\code{data} (for \code{type = "deduplication"}:}{Object of class
      \code{"data.frame"}. Data frame of original records.}

    \item{\code{data1}, \code{data2} (for \code{type = "linkage"}:}{Objects of class
      \code{"data.frame"}. Data frames of original records.}

    \item{\code{pairs}:}{Object of class \code{"data.frame"}
      Data frame of data pairs. Each row represents the comparison pattern of two records,
      identified by columns \code{id1} and \code{id2}. The other columns contain for each
      considered attribute a real number in the range [0..1] representing the degree of
      similarity. These columns are named according to the respective columns in
      \code{data}. The last column contains the matching status of the pair,
      coded as 1 for a match or 0 for a non-match.
    }

    \item{\code{frequencies}:}{Object of class \code{"numeric"}
      Numeric vector with average frequency of values for each column
      included in \code{pairs} (reciprocal of number of distinct values).
    }

    \item{\code{type}:}{Object of class \code{"character"}
      Identifies whether a linkage
      (\code{"linkage"}) or a deduplication (\code{"deduplication"}) project is
      represented.}
    \item{\code{.S3class}:}{Internal slot.}
  }
  
  The following elements are optional:
  \describe{
    \item{\code{M}:}{Object of class \code{"numeric"}
      Vector of m-probabilities as calculated by \code{\link{emWeights}}.
    }
    \item{\code{U}:}{Object of class \code{"numeric"}
      Vector of u-probabilities as calculated by \code{\link{emWeights}}.
    }
    \item{\code{W}:}{Object of class \code{"numeric"}
      Vector of log-likelihood weights as calculated by \code{\link{emWeights}},
      corresponding to binary comparison patterns as created by
      \code{\link{bincombinations}}.
    }
    \item{\code{Wdata}:}{Object of class \code{"numeric"}
      Vector of log-likelihood weights as calculated by \code{\link{emWeights}},
      corresponding to the rows of \code{pairs}.
    }
  }



}


\seealso{
  \code{"\linkS4class{RecLinkData}"} for the S4 representation.
  \code{\link{compare.dedup}}, which creates objects of this class.
  \code{"\linkS4class{RLBigData}"}, an alternative data structure suitable for
  big data sets.
}

\author{Andreas Borg, Murat Sariyar}
\keyword{classif}
