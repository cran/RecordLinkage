\name{trainSupv}
\alias{trainSupv}

\title{Train a Classifier}
\description{
  Trains a classifier for supervised classification of record pairs.
}
\usage{
trainSupv(rpairs, method, use.pred = FALSE, omit.possible = TRUE, 
  convert.na = TRUE, include.data = FALSE, ...)
}

\arguments{
  \item{rpairs}{Object of class \code{\link{RecLinkData}}. Training data.}
  \item{method}{A character vector. The classification method to use.}
  \item{use.pred}{Logical. Whether to use results of an unsupervised classification 
    instead of true matching status.}
  \item{omit.possible}{Logical. Whether to remove pairs labeled as possible
    links or with unknown status.}
  \item{convert.na}{Logical. Whether to convert \code{NA}s to 0 in the
    comparison patterns.}
  \item{include.data}{Logical. Whether to include training data in the result object.}
  \item{\dots}{Further arguments to the training method.}
}
\details{
  The given dataset is used as training data for a supervised classification.
  Either the true matching status has to be known for a sufficient number of
  data pairs or the data must have been classified previously, e.g. by using
  \code{\link{emClassify}} or \code{\link{classifyUnsup}}. In the latter case,
  argument \code{use.pred} has to be set to \code{TRUE}.
  
  A classifying method has to be provided as a character string (factors are
  converted to character) through argument \code{method}.
  The supported classifiers are:

  \describe{
    \item{\code{"svm"}}{Support vector machine, see \code{\link[e1071]{svm}}.}
    \item{\code{"rpart"}}{Recursive partitioning tree, see \code{\link{rpart}}.}
    \item{\code{"ada"}}{Stochastic boosting model, see \code{\link[ada]{ada}}.}
    \item{\code{"bagging"}}{Bagging with classification trees, see \code{\link[ipred]{bagging}}.}
    \item{\code{"nnet"}}{Single-hidden-layer neural network, see \code{\link[nnet]{nnet}}.}
    \item{\code{"bumping"}}{A bootstrap based method using classification trees, see details.}
  }
  Arguments in \code{...} are passed to the corresponding function.

  Most classifiers cannot handle \code{NA}s in the data, so by default these
  are converted to 0 before training.
  
  By \code{omit.possible = TRUE}, possible links or pairs with unknown status
  are excluded from the trainings set. Setting this argument to \code{FALSE}
  allows three-class-classification (links, non-links and possible links), but
  the results tend to be poor.
  
  Leaving \code{include.data=FALSE} saves memory, setting it to \code{TRUE}
  can be useful for saving the classificator while keeping track of the
  underlying training data.
  
  \acronym{Bumping}, (acronym for \dQuote{Bootstrap umbrella of model
  parameters}), is an ensemble method described by \cite{Tibshirani and Knight,
  1999}. Such as in bagging, multiple classifiers are trained on boostrap
  samples of the training set. The key difference is that not the aggregated
  decision of all classifiers (e.g. by majority vote) is used to classify new
  data, but only the single model that performs best on the whole training set.
  In combination with classification trees as underlying classifiers this
  approach allows good interpretability of the trained model while being more
  stable against outliers than traditionally induced decision trees. The number
  of bootstrap samples to use can be controlled by supplying the argument
  \code{n.bootstrap}, which defaults to 25.
}

\value{
  An object of class \code{RecLinkClassif} with the following components:
  \item{train}{If \code{include.data} is \code{TRUE}, a copy of \code{rpairs},
    otherwise an empty data frame with the same column names.}
  \item{model}{The model returned by the underlying training function.}
  \item{method}{A copy of the argument \code{method}.}
}

\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{classifySupv}} for classifying with the trained model, 
  \code{\link{classifyUnsup}} for unsupervised classification}

\references{
Tibshirani R, Knight K: Model search by bootstrap \dQuote{bumping}.
Journal of Computational and Graphical Statistics 8(1999):671--686.
}
\examples{
# Train a rpart decision tree with additional parameter minsplit
data(RLdata500)
pairs=compare.dedup(RLdata500, identity=identity.RLdata500,
                    blockfld=list(1,3,5,6,7))
model=trainSupv(pairs, method="rpart", minsplit=5)
summary(model)
}
\keyword{classif}
