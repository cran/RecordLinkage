\name{classifySupv}
\alias{classifySupv}
\alias{classifySupv-methods}
\alias{classifySupv,RecLinkClassif,RecLinkData-method}
\alias{classifySupv,RecLinkClassif,RLBigData-method}

\title{Supervised Classification}
\description{
  Supervised classification of record pairs based on a trained model.
}
\usage{

  classifySupv(model, newdata, ...)

  \S4method{classifySupv}{RecLinkClassif,RecLinkData}(model, newdata,
    convert.na = TRUE, ...)

  \S4method{classifySupv}{RecLinkClassif,RLBigData}(model, newdata,
    convert.na = TRUE, withProgressBar = (sink.number()==0), ...)
}

\arguments{
  \item{model}{Object of class \code{RecLinkClassif}. The
    calibrated model. See \code{\link{trainSupv}}.}
  \item{newdata}{Object of class \code{"\link{RecLinkData}"}
    or \code{"\linkS4class{RLBigData}"}. The data to classify.}
  \item{convert.na}{Logical. Whether to convert missing values in the comparison
    patterns to 0.}
  \item{withProgressBar}{Whether to display a progress bar}
  \item{\dots}{Further arguments for the \code{\link{predict}} method.}
}

\details{

  The record pairs in \code{newdata} are classified by calling
  the appropriate \code{\link{predict}} method for \code{model$model}.

  By default, the \code{"\linkS4class{RLBigDataDedup}"} method displays a
  progress bar unless output is diverted by \code{sink}, e.g. when processing
  a Sweave file.

}

\value{
  For the \code{"\link{RecLinkData}"} method, a S3 object
  of class \code{"\link{RecLinkResult}"} that represents a copy
  of \code{newdata} with element \code{rpairs$prediction}, which stores
  the classification result, as addendum.

  For the \code{"\linkS4class{RLBigData}"} method, a S4 object of class
  \code{"\linkS4class{RLResult}"}.
}

\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{trainSupv}} for training of classifiers,
         \code{\link{classifyUnsup}} for unsupervised classification.}

\examples{
# Split data into training and validation set, train and classify with rpart
data(RLdata500)
pairs=compare.dedup(RLdata500, identity=identity.RLdata500,
                    blockfld=list(1,3,5,6,7))
l=splitData(pairs, prop=0.5, keep.mprop=TRUE)                    
model=trainSupv(l$train, method="rpart", minsplit=5)
result=classifySupv(model=model, newdata=l$valid)
summary(result)

}


\keyword{classif}
