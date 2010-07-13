\name{classifyUnsup}
\alias{classifyUnsup}

\title{Unsupervised Classification}
\description{
  Classify record pairs with unsupervised clustering methods.
}

\usage{
classifyUnsup(rpairs, method, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rpairs}{Object of type \code{\link{RecLinkData}}. The data to
    classify.}
  \item{method}{The classification method to use. One of \code{"kmeans"},
    \code{"bclust"}.}
  \item{\dots}{Further arguments for the classification method}
}
\details{
  A clustering algorithm is applied to find clusters in the record pairs. In the 
  case of   two clusters (the default), the cluster further from the origin 
  (i.e. representing higher similarity values) is   interpreted as the set of links, 
  the other as the set of non-links.
  
  Supported methods are:
  \describe{
    \item{kmeans}{K-means clustering, see \code{\link[stats]{kmeans}}.}
    \item{bclust}{Bagged clustering, see \code{\link[e1071]{bclust}}.}
  }
}

\value{
  An object of class \code{\link{RecLinkResult}} with the classification
  result stored in the \code{prediction} component, otherwise a copy of
  \code{rpairs}.
}

\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{trainSupv}} and \code{\link{classifySupv}} for supervised
  classification.}

\examples{
# Classification with bclust
data(RLdata500)
rpairs=compare.dedup(RLdata500, identity=identity.RLdata500,
                    blockfld=list(1,3,5,6,7))
result=classifyUnsup(rpairs,method="bclust")
summary(result)                    
}

\keyword{classif}
