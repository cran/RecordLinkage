\name{genSamples}
\alias{genSamples}

\title{Generate Training Set}
\description{
  Generates training data by unsupervised classification.
}
\usage{
genSamples(dataset, num.non, des.mprop = 0.1)
}

\arguments{
  \item{dataset}{Object of class \code{\link{RecLinkData}}. Data pairs from 
                   which to sample.}
  \item{num.non}{Positive Integer. Number of desired non-links in the training set.}
  \item{des.mprop}{Real number in the range [0,1]. Ratio of number of links to
    number of non-links in the training set.}
}
\details{
  The application of supervised classifiers (via \code{\link{classifySupv}})
  requires a training set of record pairs with known matching status.
  Where no such data are available, \code{genSamples} can be used to generate
  training data. The matching status is determined by unsupervised
  clustering with \code{\link[e1071]{bclust}}. Subsequently, the desired number of 
  links and  non-links are sampled.
  
  If the requested numbers of matches or non-matches is not feasible, a
  warning is issued and the maximum possible number is considered.
}
\value{
  A list of \code{\link{RecLinkResult}} objects.
  \item{train}{The sampled training data.}
  \item{valid}{All other record pairs}
  Record pairs are split into the respective \code{pairs} components.
  The \code{prediction} components represent the clustering result. If weights are
  present in \code{dataset}, the corresponding values of \code{Wdata} are
  stored to \code{train} and \code{valid}. All other components are copied
  from \code{dataset}.
}

\author{Andreas Borg, Murat Sariyar}
\note{
  Unsupervised clustering may lead to a poor quality of classification, all
  subsequent results should be evaluated critically.
}
\seealso{\code{\link{splitData}} for splitting data sets without clustering.}

\keyword{classif}
