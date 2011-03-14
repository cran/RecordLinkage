\name{strcmp}
\alias{strcmp}
\alias{jarowinkler}
\alias{jaro}
\alias{winkler}
\alias{levenshtein}
\alias{levenshteinDist}
\alias{levenshteinSim}

\title{String Metrics}

\description{Functions for computation of the similarity between two strings.}

\usage{jarowinkler(str1, str2, W_1=1/3, W_2=1/3, W_3=1/3, r=0.5)
levenshteinSim(str1, str2)
levenshteinDist(str1, str2)
}

\arguments{
   \item{str1,str2}{Two character vectors to compare.}
   \item{W_1,W_2,W_3}{Adjustable weights.}
   \item{r}{Maximum transposition radius. A fraction of the length of the
            shorter string.}
}



\details{
  String metrics compute a similarity value in the range \eqn{[0,1]} for two
  strings, with 1 denoting the highest (usually equality) and 0 denoting the
  lowest degree of similarity. In the context of Record Linkage, string
  similarities can improve the discernibility between matches and non-matches.
  
  \code{jarowinkler} is an implementation of the algorithm by Jaro and Winkler
  (see references). For the meaning of \code{W_1}, \code{W_2}, \code{W_3} and
  \code{r} see the referenced article. For most applications, the default
  values are reasonable.

  \code{levenshteinDist} returns the Levenshtein
  distance, which cannot be directly used as a valid string comparator.
  \code{levenshteinSim} is a similarity function based on
  the Levenshtein distance, calculated by
  \eqn{1-\frac{\mathrm{d}(\mathit{str}_{1},\mathit{str}_{2})}{\max(A,B))}}{
  1 - d(str1,str2) / max(A,B)}, where \eqn{\mathrm{d}}{d} is the Levenshtein distance
  function and \eqn{A} and \eqn{B} are the lenghts of the strings.

  Arguments \code{str1} and \code{str2} are expected to be of type 
  \code{"character"}.
  Non-alphabetical characters can be processed. Valid format combinations for
  the arguments are:
  \itemize{
    \item Two arrrays with the same dimensions.
    \item Two vectors. The shorter one is recycled as necessary.
  }
}

\value{A numeric vector with similarity values in the interval
  \eqn{[0,1]}{[0,1]}. For \code{levenshteinDist}, the edit distance as an
  integer vector.
}

\note{String comparison is case-sensitive, which means that for example
\code{"R"} and \code{"r"} have a similarity of 0. If this behaviour is undesired,
strings should be normalized before processing.}

\references{Winkler, W.E.: String Comparator Metrics and Enhanced Decision
Rules in the Fellegi-Sunter Model of Record Linkage. In: Proceedings
of the Section on Survey Research Methods, American Statistical Association
(1990), S. 354--369.}

\author{Andreas Borg, Murat Sariyar}

\examples{
# compare two strings:
jarowinkler("Andreas","Anreas")
# compare one string with several others:
levenshteinSim("Andreas",c("Anreas","Andeas"))
# compare two vectors of strings:
jarowinkler(c("Andreas","Borg"),c("Andreas","Bork"))
}

\keyword{misc}

