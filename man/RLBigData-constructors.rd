\name{RLBigDataDedup}
\alias{RLBigDataDedup}
\alias{RLBigDataLinkage}

\title{
  Constructors for big data objects.
}
\description{
  These are constructors which initialize a record linkage setup for
  big datasets, either deduplication of one (\code{RLBigDataDedup})
  or linkage of two datasets (\code{RLBigDataLinkage}).
}

\usage{
RLBigDataDedup(dataset, identity = NA, blockfld = list(), exclude = numeric(0), 
  strcmp = numeric(0), strcmpfun = "jarowinkler", phonetic = numeric(0), 
  phonfun = "pho_h")

RLBigDataLinkage(dataset1, dataset2, identity1 = NA, identity2 = NA, 
  blockfld = list(), exclude = numeric(0), strcmp = numeric(0), 
  strcmpfun = "jarowinkler", phonetic = numeric(0), phonfun = "pho_h")
}
\arguments{
  \item{dataset, dataset1, dataset2}{Table of records to be deduplicated or linked.
    Either a data frame or a matrix.}

  \item{identity, identity1, identity2}{Optional vectors (are converted to
              factors) for identifying true matches and
              non-matches. In a deduplication process, two records \code{dataset[i,]}
               and \code{dataset[j,]} are a true match if and only if 
              \code{identity[i,]==identity[j,]}. In a linkage process, two 
              records \code{dataset1[i,]} and \code{dataset2[j,]} are a true 
              match if and only if \code{identity1[i,]==identity2[j,]}.}

  \item{blockfld}{Blocking field definition. A numeric or character
                  vector or a list of several such vectors,
                  corresponding to column numbers or names. 
                  See details and examples.}

  \item{exclude}{Columns to be excluded. A numeric or character vector
                  corresponding to columns of dataset or dataset1 and dataset2
                  which should be excluded from comparision}

  \item{strcmp}{Determines usage of string comparison. If \code{FALSE}, no
                  string comparison will be used; if \code{TRUE}, string comparison
                  will be used for all columns; if a numeric or character vector 
                  is given, the string comparison will be used for the specified columns.}

  \item{strcmpfun}{Character string representing the string comparison function.
                Possible values are \code{"jarowinkler"} and \code{"levenshtein"}.
  }
  
  \item{phonetic}{Determines usage of phonetic code. Used in the same manner as
                  \code{strcmp}}.

  \item{phonfun}{Character string representing the phonetic function. Currently,
                only \code{"pho_h"} is supported (see \code{\link{pho_h}}).
  }
}
\details{
  These functions act as constructors for the S4 classes
  \code{"\linkS4class{RLBigDataDedup}"} and \code{"\linkS4class{RLBigDataLinkage}"}.
  They make up the initial stage in a Record Linkage process using
  large data sets (>= 1.000.000 record pairs) after possibly
  normalizing the data. Two general
  scenarios are reflected by the two functions: \code{RLBigDataDedup} works on a
  single data set which is to be deduplicated, \code{RLBigDataLinkage} is intended
  for linking two data sets together. Their usage follows the functions
  \code{\link{compare.dedup}} and \code{\link{compare.linkage}}, which are recommended
  for smaller amounts of data, e.g. training sets.

  Datasets are represented as data frames or matrices (typically of type
  character), each row representing one record, each column representing one
  attribute (like first name, date of birth,\ldots). Row names are not
  retained in the record pairs. If an identifier other than row number is
  needed, it should be supplied as a designated column and excluded from
  comparison (see note on \code{exclude} below).
  
  In case of \code{RLBigDataLinkage}, the two datasets must have the same number
  of columns and it is assumed that their column classes and semantics match.
  If present, the column names of \code{dataset1} are assigned to \code{dataset2}
  in order to enforce a matching format. Therefore, column names used in
  \code{blockfld} or other arguments refer to \code{dataset1}.


  Each element of \code{blockfld} specifies a set of columns in which two
  records must agree to be included in the output. Each blocking definition in
  the list is applied individually, the sets obtained
  thereby are combined by a union operation.
  If \code{blockfld} is \code{FALSE}, no blocking will be performed,
  which leads to a large number of record pairs
  (\eqn{\frac{n(n-1)}{2}}{n*(n-1)/2} where \eqn{n} is the number of
  records).

  Fields can be excluded from the linkage process by supplying their column
  index in the vector \code{exclude}, which is espacially useful for
  external identifiers. Excluded fields can still be used for
  blocking, also with phonetic code.

  Phonetic codes and string similarity measures are supported for enhanced
  detection of misspellings. Applying a phonetic code leads to binary
  similarity values, where 1 denotes equality of the generated phonetic code.
  A string comparator leads to a similarity value in the range \eqn{[0,1]}.
  Using string comparison on a field for which a phonetic code
  is generated is possible, but issues a warning.
  
  In contrast to the \code{compare.*} functions, phonetic coding and string
  comparison is not carried out in R, but by database functions. Supported
  functions are \code{"pho_h"} for phonetic coding and \code{"jarowinkler"} and
  \code{"levenshtein"} for string comparison. See the documentation for their
  R equivalents (\link[=phonetics]{phonetic functions},
  \link[=strcmp]{string comparison}) for further information.
}
\value{
  An object of class \code{"\linkS4class{RLBigDataDedup}"} or
  \code{"\linkS4class{RLBigDataLinkage}"}, depending on the called function.
}

\section{Side effects}{
  The RSQLite database driver is initialized via \code{dbDriver("SQLite")}
  and a connection established and stored in the returned object. Extension
  functions for phonetic code and string comparison are loaded into the database.
  The records in \code{dataset} or \code{dataset1} and \code{dataset2} are stored in tables
  \code{"data"} or \code{"data1"} and \code{"data2"}, respectively, and 
  indices are created on all columns involved in blocking.
}

\author{
  Andreas Borg, Murat Sariyar
}


\seealso{
  \code{"\linkS4class{RLBigDataDedup}"}, \code{"\linkS4class{RLBigDataLinkage}"},
  \code{\link{compare.dedup}}, \code{\link{compare.linkage}},
  the vignette "Classes for record linkage of big data sets".
}
\examples{
data(RLdata500)
data(RLdata10000)
# deduplication without blocking, use string comparator on names
rpairs <- RLBigDataDedup(RLdata500, strcmp = 1:4)
# linkage with blocking on first name and year of birth, use phonetic
# code on first components of first and last name
rpairs <- RLBigDataLinkage(RLdata500, RLdata10000, blockfld = c(1, 7),
  phonetic = c(1, 3))
# deduplication with blocking on either last name or complete date of birth,
# use string comparator on all fields, include identity information
rpairs <- RLBigDataDedup(RLdata500, identity = identity.RLdata500, strcmp=TRUE,
  blockfld = list(1, c(5, 6, 7)))

}
\keyword{classif}

