\name{classifySupv}
\alias{classifySupv}

\title{Supervised Classification}
\description{
  Supervised classification of record pairs based on a trained model.
}
\usage{
classifySupv(model, newdata, ...)
}

\arguments{
  \item{model}{Object of type \code{RecLinkClassif}. The
    trained model. See \code{\link{trainSupv}}.}
  \item{newdata}{Object of type \code{\link{RecLinkData}}. The data to
    classify.}
  \item{\dots}{Further arguments for the \code{\link{predict}} function.}
}

\details{

  The record pairs in \code{newdata} are classified by calling
  the appropriate \code{\link{predict}} function for \code{model$model}.
  
}

\value{
  A \code{\link{RecLinkResult}} object which is a copy of \code{newdata},
  except for element \code{rpairs$prediction}, which stores the classification
  result.
}

\author{Andreas Borg}

\seealso{\code{\link{trainSupv}} for training of classifiers,
         \code{\link{classifyUnsup} for unsupervised classification.}
         }
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