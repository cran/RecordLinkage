\name{compare}
\alias{compare.dedup}
\alias{compare.linkage}
\title{Compare Records}
\description{Builds comparison patterns of record pairs for deduplication or
  linkage.}
\usage{

compare.dedup (dataset, blockfld = FALSE, phonetic = FALSE, 
  phonfun = pho_h, strcmp = FALSE, strcmpfun = jarowinkler, exclude = FALSE,
  identity = NA, n_match = NA, n_non_match = NA)

compare.linkage (dataset1, dataset2, blockfld = FALSE, 
  phonetic = FALSE, phonfun = pho_h, strcmp = FALSE, 
  strcmpfun = jarowinkler, exclude = FALSE, identity1 = NA, identity2 = NA,
  n_match = NA, n_non_match = NA)
}

\arguments{
  \item{dataset}{Table of records to be deduplicated. Either a data frame or 
                 a matrix.} 
  \item{dataset1, dataset2}{Two data sets to be linked.}
  \item{blockfld}{Blocking field definition. A list of integer or character vectors
                  with column indices or \code{FALSE} to disable
                  blocking. See details and examples.}
  \item{phonetic}{Determines usage of a phonetic code. If \code{FALSE}, no
                  phonetic code will be used; if \code{TRUE}, the phonetic code
                  will be used for all columns; if a numeric or character vector is given, the
                  phonetic code will be used for the specified columns.}
  \item{phonfun}{Function for phonetic code. See details.}
  \item{strcmp}{Determines usage of a string metric. Used in the same manner
                as \code{phonetic}}
  \item{strcmpfun}{User-defined function for string metric. See details.}
  \item{exclude}{Columns to be excluded. A numeric or character vector specifying
                  the columns
                  which should be excluded from comparision}
  \item{identity, identity1, identity2}{Optional numerical vectors for identifying matches and
              non-matches. In a deduplication process, two records \code{dataset[i,]}
               and \code{dataset[j,]} are a true match if and only if 
              \code{identity[i,]==identity[j,]}. In a linkage process, two 
              records \code{dataset1[i,]} and \code{dataset2[j,]} are a true 
              match if and only if \cr \code{identity1[i,]==identity2[j,]}.}
  \item{n_match, n_non_match}{Number of desired matches and non-matches in
      the result.}
}

\value{An object of class \code{RecLinkPairs} with the following components:
  \item{data}{Copy of the records, converted to a data frame.}
  \item{pairs}{Generated comparison patterns.}
  \item{frequencies}{For each column included in \code{pairs}, the average
    frequency of values (reciprocal of number of distinct values).}   
}

\details{
  These functions build record pairs and finally comparison patterns
  by which these pairs are later classified as links or non-links. They make up
  the initial stage in a Record Linkage process after possibly 
  normalizing the data. Two general
  scenarios are reflected by the two functions: \code{compare.dedup} works on a
  single data set which is to be deduplicated, \code{compare.linkage} is intended
  for linking two data sets together.
  
  Data sets are represented as data frames or matrices (typically of type 
  character), each row representing one record, each column representing one
  field or attribute (like first name, date of birth\ldots). Row names are not
  retained in the record pairs. If an identifier other than row number is
  needed, it should be supplied as a designated column and excluded from
  comparison (see note on \code{exclude} below).
  
  Each element of \code{blockfld} specifies a set of columns in which two
  records must agree to be included in the output. Each blocking definition in
  the list is applied individually, the sets obtained 
  thereby are combined by a union operation.                              
  If \code{blockfld} is \code{FALSE}, no blocking will be performed,
  which leads to a large number of record pairs 
  (\eqn{\frac{n(n-1)}{2}}{n*(n-1)/2} where \eqn{n} is the number of
  records).
  
  As an alternative to blocking, a determined number of \code{n_match} matches 
  and \code{n_non_match} non-matches can be drawn if \code{identity} or
  \code{identity1} and \code{identity2} are supplied. This is relevant for
  generating training sets for the supervised classificators (see 
  \code{\link{trainSupv}}).
  
  Fields can be excluded from the linkage process by supplying their column
  index in the vector \code{exclude}, which is espacially useful for
  external identifiers. Excluded fields can still be used for
  blocking, also with phonetic code.
  
  Phonetic codes and string similarity measures are supported for enhanced 
  detection of misspellings. Applying a phonetic code leads to a binary
   values, where 1 denotes equality of the generated phonetic code.
  A string comparator leads to a similarity value in the range \eqn{[0,1]}.
  String comparison is not allowed on a field for which a phonetic code
  is generated. For phonetic encoding functions included in the package, 
  see \link{phonetics}. For the included string comparators, see 
  \code{\link{jarowinkler}} and \code{\link{levenshteinSim}}.
  Please note that phonetic code and string 
  metrics can slow down the generation of comparison patterns significantly.
  
  User-defined functions for phonetic code and string comparison can be supplied
  via the arguments \code{phonfun} and \code{strcmpfun}. \code{phonfun} is 
  expected to have a single character argument (the string to be transformed) and must
  return a character value with the encoded string. 
  \code{strcmpfun} must have as arguments the two strings to be compared and
  return a similarity value in the range \eqn{[0,1]}, with 0 denoting the lowest 
  and 1 denoting the highest degree of similarity. Both
  functions must be fully vectorized to work on matrices.
  
  
}


\seealso{
  \code{\link{RecLinkData}} for the format of returned objects,
%-  \code{\link{genSamples}} for automatic generation of training data.
}

\author{Andreas Borg, Murat Sariyar}

\examples{
data(RLdata500)
data(RLdata10000)

# deduplication without blocking, use string comparator on names
\dontrun{rpairs=compare.dedup(RLdata500,strcmp=1:4)}
# linkage with blocking on first name and year of birth, use phonetic
# code on first components of first and last name
rpairs=compare.linkage(RLdata500,RLdata10000,blockfld=c(1,7),phonetic=c(1,3))
# deduplication with blocking on either last name or complete date of birth,
# use string comparator on all fields, include identity information
rpairs=compare.dedup(RLdata500, identity=identity.RLdata500, strcmp=TRUE,
  blockfld=list(1,c(5,6,7)))
# Draw 100 matches and 1000 non-matches
\dontrun{rpairs=compare.dedup(RLdata10000,identity=identity.RLdata10000,n_match=100,
  n_non_match=10000)}
}
\keyword{classif}
