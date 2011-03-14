\encoding{latin1}
\name{getParetoThreshold}
\Rdversion{1.1}
\alias{getParetoThreshold}

\title{
  Estimate Threshold from Pareto Distribution
}
\description{
  Calculates a classification threshold based on a generalized Pareto 
  distribution (GPD) fitted to the weights distribution of the given data pairs.
}

\usage{
getParetoThreshold(rpairs, quantil = 0.95, interval = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rpairs}{
    A \code{\link{RecLinkData}} object with weights. The data for which to
    compute a threshold.
}

  \item{quantil}{
    A real number between 0 and 1. The quantil which to compute.
}
  \item{interval}{
    A numeric vector denoting the interval on which to fit
    a GPD.
}
}

\details{
  This threshold calculation is based on the assumption that the distribution 
  of weights exihibit a `fat tail' which can be fitted by a generalized Pareto 
  distribution (GPD). The limits of the interval which is subject to the
  fitting are usually determined by reviewing a mean residual life plot of
  the data. If the limits are not externally supplied, a MRL plot is displayed 
  from which the endpoints   can be selected by mouse input. If only one endpoint 
  is selected or supplied, the greater endpoint is set to the maximum weight.
  
  A suitable interval is characterized by a relatively long, approximately 
  linear segment of the plot. 
}

\value{
  A classification threshold.
}
\references{
  Sariyar M., Borg A. and Pommerening M.: Controlling false match rates in
  record linkage using extreme value theory. Journal of Biomedical Informatics
  (article in press), \url{http://dx.doi.org/10.1016/j.jbi.2011.02.008}.
}
\author{
  Andreas Borg, Murat Sariyar
}
\note{
  The quality of matching varies, poor results can occur in some cases. 
  Evaluate carefully before applying to a real case.
}



\seealso{
  \code{\link{emWeights}} and \code{\link{epiWeights}} for calculating weights,
  \code{\link{emClassify}} and \code{\link{epiClassify}} for classifying with
  the returned threshold.
}
\examples{
  data(RLdata500)
  rpairs=compare.dedup(RLdata500, identity=identity.RLdata500, strcmp=TRUE,
    blockfld=list(1,3,5,6,7))
  rpairs=epiWeights(rpairs)
  # leave out argument interval to choose from plot
  threshold=getParetoThreshold(rpairs,interval=c(0.68, 0.79))
  summary(epiClassify(rpairs,threshold))
}
\keyword{models}
\keyword{classif}
