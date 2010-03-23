\name{summary}

\alias{summary.RecLinkData}
\alias{summary.RecLinkResult}

\title{Print Summary of Record Linkage Data}

\description{Prints information on \code{\link{RecLinkData}} and
  \code{\link{RecLinkResult}} objects.}

\usage{
\method{summary}{RecLinkData}(object,...)

\method{summary}{RecLinkResult}(object,...)                    
}

\arguments{
  \item{object}{The object for which to print a summary.} 
  \item{...}{Additional arguments from the generic, silently ignored.}
}                  

\details{

       The printed information for \code{\link{RecLinkData}} objects
       includes:
      
       \itemize{
        \item The number of records.
        \item The number of record pairs.
        \item The number of true matches, true non-matches and pairs with unknown
          status.
        \item If weights have been calculated for this object, a textual histogram
          of the weight distribution.            
       }
       Information on \code{\link{RecLinkResult}} objects includes all of the
       above and the following:
       \itemize{
          \item The number of detected links, non-links and possible links.
          
          \item Error rates, if the true matching status of all record pairs is
            known. Alpha error is the ratio of false links
            to matches, beta error is the ratio of false non-links to
            non-matches and accuracy the ratio of correctly classified
            pairs to the total number of pairs.
            
          \item A cross-classified table counting true matching status against
            classification. The true matching status is represented as logical
            values, possibly including \code{NA} for unknown status.
            Classification results are represented by:
            \describe{
              \item{\code{"L"}}{for links,}
              \item{\code{"P"}}{for possible links}
              \item{\code{"N"}}{for non-links}
            }

      }
}

\value{Used for its side effect.}

\author{Andreas Borg}

\seealso{\code{\link{RecLinkData}},\code{\link{RecLinkResult}}}

\keyword{classif}