\name{mygllm}
\alias{mygllm}

\title{Generalized Log-Linear Fitting}

\description{
  Fits a log-linear model for incomplete contingency tables.
}
\usage{
mygllm(y, s, X, maxit = 1000, tol = 1e-05, E = rep(1, length(s)))
}

\arguments{
 \item{y}{Vector of observed cell frequencies.}
 \item{s}{Scatter matrix. s[i] is the cell in the observed array that
          corresponds to cell i in the full array.}
 \item{X}{Design matrix.}
 \item{maxit}{Maximum number of iterations.}
 \item{tol}{Convergence parameter.}
 \item{E}{Full contingency table. Should be initialized with either ones or
          a priori estimates.}
}

\details{This is an implementation and extension of the algorithm published by
  Haber (1984). It also incorporates ideas of David Duffy (see references).

  A priori estimates of the full contingency table can be given as
  start values by argument \code{E}. This can reduce
  execution time significantly.        
}

\value{Estimated full contingency table.}


\references{Michael Haber, Algorithm AS 207: Fitting a General Log-Linear
            Model, in: Applied Statistics 33 (1984) No. 3, 358--362.
            
            David Duffy: gllm: Generalised log-linear model. R package
            version 0.31. see \url{http://www.qimr.edu.au/davidD/#loglin}}

\author{Andreas Borg}

\seealso{\code{\link{emWeights}}, which makes use of log-linear fitting for
  weight calculation.}
