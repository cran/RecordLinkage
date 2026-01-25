\name{splitData}
\alias{splitData}

\title{Split Data}
\description{
  Splits a data set into two sets with desired proportions.
}

\usage{
splitData(dataset, prop, keep.mprop = FALSE, num.non = 0, des.mprop = 0, 
use.pred = FALSE)
}

\arguments{
  \item{dataset}{Object of class \code{\link{RecLinkData}}. Data pairs to split.}
  \item{prop}{Real number between 0 and 1. Proportion of data pairs to form the training set.}
  \item{keep.mprop}{Logical. Whether the ratio of matches should be retained.}
  \item{num.non}{Positive Integer. Desired number on non-matches in the training set.}
  \item{des.mprop}{Real number between 0 and 1. Desired proportion of matches to
                  non-matches in the training set.}
  \item{use.pred}{Logical. Whether to apply match ratio to previous classification 
                 results instead of true matching status.}
}

\value{
  A list of \code{\link{RecLinkData}} objects.
  \item{train}{The sampled training data.}
  \item{valid}{All other record pairs}

  The sampled data are stored in the \code{pairs} attributes of \code{train} 
  and \code{valid}. If present, the attributes \code{prediction} and \code{Wdata} 
  are split and the corresponding values saved. All other attributes are
  copied to both data sets.
  
  If the number of desired matches or non-matches is higher than the number
  actually present in the data, the maximum possible number is chosen and a
  warning issued.
}

\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{genSamples}} for generating training data based on 
          unsupervised classification.}
\examples{
data(RLdata500)
pairs=compare.dedup(RLdata500, identity=identity.RLdata500, 
  blockfld=list(1,3,5,6,7))

# split into halves, do not enforce match ratio
l=splitData(pairs, prop=0.5)
summary(l$train)
summary(l$valid)

# split into 1/3 and 2/3, retain match ration
l=splitData(pairs, prop=1/3, keep.mprop=TRUE)
summary(l$train)
summary(l$valid)

# generate a training set with 100 non-matches and 10 matches
l=splitData(pairs, num.non=100, des.mprop=0.1, keep.mprop=TRUE)
summary(l$train)
summary(l$valid)

}

\keyword{classif}
