\name{getMinimalTrain}
\Rdversion{1.1}
\alias{getMinimalTrain}
\alias{getMinimalTrain-methods}
\alias{getMinimalTrain,RecLinkData-method}
\alias{getMinimalTrain,RLBigData-method}


\title{
  Create a minimal training set
}
\description{
  Samples a subset of the provided data (comparison patterns) so that every
  comparison pattern in \code{rpairs} is represented in the subset
  at least once.
}

\usage{
getMinimalTrain(rpairs, nEx = 1)
}

\arguments{
  \item{rpairs}{
    A \code{"RecLinkData"} or \code{"RLBigData"} object. The data set
    from which to create a minimal training set.
}
  \item{nEx}{
    The desired number of examples per comparison pattern.
}
}

\details{
  Our internal research has given indication that in the context of Record Linkage 
  with supervised classification procedures small training sets are often 
  sufficient, provided they cover the whole range of present comparison patterns.
  By default, this function creates a minimal training set that is
  a subset of the record pairs to be classified in which every present 
  comparison pattern is represented by exactly one training example.
  By this approach, the work to classify a training set by
  clerical review can be minimized while keeping a good classification 
  performance.
  
  Larger training sets can be obtained by setting \code{nEx} to a 
  higher number. Up to \code{nEx} examples for every comparison pattern
  are randomly selected, limited by the total number of record pairs with
  that pattern.

}

\value{
  An object of the same class as \code{rpairs}, representing a minimal
  comprehensive training set. The appropriate subset of comparison patterns
  (and weights, if present) is taken, all other components are copied.
}

\note{
  Application is only advisable for binary comparison patterns (i.e. only 0
  and 1 appear as agreement values). For patterns with string comparison
  values, the size of the returned set can be too large for a manual review.
  A warning is issued if fuzzy agreement values (\eqn{>0} and \eqn{<1}) are
  present in the data.
}


\author{
Andreas Borg, Murat Sariyar
}
\note{
  Due to the small size of the resulting training set, outliers can have a
  relatively high impact on further classification results. Stable methods
  such as Bagging or Support-Vector-Machines should be used in conjunction
  with minimal training sets to minimize this risk.
}


\seealso{
  \code{\link{editMatch}} for manually setting the matching status of the
  training pairs.
}
\examples{
data(RLdata500)
p <- compare.dedup(RLdata500,blockfld=list(1,3),identity=identity.RLdata500)
train <- getMinimalTrain(p)
classif <- trainSupv(train,method="bagging")
summary(classifySupv(classif,newdata=p))
}

\keyword{classif}

