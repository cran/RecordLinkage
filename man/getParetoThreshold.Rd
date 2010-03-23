\encoding{latin1}
\name{getParetoThreshold}
\Rdversion{1.1}
\alias{getParetoThreshold}

\title{
  Estimate Threshold from Pareto Distribution
}
\description{
  Calculates a classification threshold based on a generalized Pareto 
  distribution (GPD) fitted to the weights of the given data pairs.
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
  This threshold calculation is based on the assumption that weights in
  the `middle' range (the region of 'possible links' in classical Record 
  Linkage) form a `fat tail' and can be fitted to a generalized Pareto 
  distribution (GPD). The limits of the interval which is subject to
  fitting are usually determined by reviewing a mean residual life plot of
  the data. If not supplied, a MRL plot is displayed from which the endpoints
  can be selected by mouse input. If only one endpoint is selected or supplied,
  the greater endpoint is set to the maximum weight.
  
  A suitable interval is characterized by a relatively long, approximately 
  linear segment of the plot. For weights computed by \code{\link{emWeights}},
  it is usally located around 0, for weights computed by 
  \code{\link{epiWeights}} between \eqn{0.5} and \eqn{1}.
}

\value{
  The resulting threshold.
}
\references{
  Sariyar et al.: Bestimmung der False Match-Rate im Fellegi-Sunter-Modell 
  mittels verallgemeinerte Paretoverteilung, Presentation for
  54. Jahrestagung der Deutschen Gesellschaft für Medizinische 
  Informatik, Biometrie und Epidemiologie e.V. (GMDS).
}
\author{
  Andreas Borg
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
