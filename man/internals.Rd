\name{internals}
\alias{begin}
\alias{begin-methods}
\alias{begin,RLBigData-method}
\alias{nextPairs}
\alias{nextPairs-methods}
\alias{nextPairs,RLBigData-method}
\alias{clear}
\alias{clear-methods}
\alias{clear,RLBigData-method}
\alias{blockfldfun}
\alias{getSQLStatement}
\alias{getSQLStatement-methods}
\alias{getSQLStatement,RLBigData-method}
\alias{init_sqlite_extensions}
\alias{.allows_extensions}
\alias{.lib_path}
\alias{getPatternCounts}
\alias{getPatternCounts-methods}
\alias{getPatternCounts,RLBigData-method}
\alias{getMatchCount}
\alias{getMatchCount-methods}
\alias{getMatchCount,RLBigData-method}
\alias{getNACount}
\alias{getNACount-methods}
\alias{getNACount,RLBigData-method}
\alias{getDbFile}
\alias{getDbFile-methods}
\alias{getDbFile,RLBigData-method}
\alias{getDbFile,RLResult-method}
\alias{getColumnNames}
\alias{getColumnNames-methods}
\alias{getColumnNames,RLBigDataDedup-method}
\alias{getColumnNames,RLBigDataLinkage-method}
\alias{getThresholds}
\title{
  Internal functions and methods
}
\description{
  These functions and methods are used internally and should usually not
  be called from outside the package.
}
\usage{
  begin(x, ...)
  nextPairs(x, n = 10000, ...)
  clear(x, ...)
  blockfldfun(blockfld, phoneticFld, phoneticFun, coln)
  getSQLStatement(object)
  init_sqlite_extensions(db)
  .allows_extensions(db)
  .lib_path()
  getPatternCounts(x, n=10000, cutoff=1)
  getMatchCount(object)
  getNACount(object)
  getDbFile(object)
  getColumnNames(object, withExcluded = FALSE)
  getThresholds(W, M, U, my, ny)
}

\arguments{
  \item{x, object}{A \code{"\linkS4class{RLBigData}"} object (also
    \code{"\linkS4class{RLResult}"} for \code{getDbFile}.}
  \item{n}{Maximum number of comparison patterns to fetch.}
  \item{blockfld}{Blocking definition as in
    \code{\link[=RLBigDataDedup]{RLBigData*}}.}
  \item{phoneticFld}{Indices of attributes for which phonetic code should be used.}
  \item{phoneticFun}{Name of phonetic function as in
    \code{\link[=RLBigDataDedup]{RLBigData*}}.}
  \item{coln}{Column names of records.}
  \item{db}{Database connection.}
  \item{cutoff}{Threshold from which string comparison values are rounded to 1.}
  \item{withExcluded}{Logical. Wether to output names of excluded columns}
  \item{W,M,U}{Linkage weights, m- and u-probabilites. See \code{\link{emWeights}}
    for details.}
  \item{W,M,U}{Error bounds. See \code{\link{emClassify}} for details.}
  \item{\dots}{Optional arguments.}
}

\details{
  \describe{
    \item{\code{begin}}{Sends a query to the database of an \code{"RLBigData"}
      object which creates comparison patterns.}
    \item{\code{nextPairs}}{Retreives the next \code{n} comparison patterns.}
    \item{\code{clear}}{Closes the result set opened by \code{begin}.}
    \item{\code{blockfldfun}}{Constructs the part of SQL statement which
      implements blocking.}
    \item{\code{blockfldfun}}{Constructs the part of SQL statement which
      implements blocking.}
    \item{\code{getSQLStatement}}{Constructs SQL statement for retreiving
      comparison patterns.}
    \item{\code{init_sqlite_extensions}}{Load string comparison and phonetic
      functions into database.}
    \item{\code{.allows_extensions}}{Tests wether SQLite engine supports
      extension functions.}
    \item{\code{.lib_path}}{Gets path of shared library of package.}
    \item{\code{getPatternCounts}}{Counts binary patterns, implemented through
      \code{\link{countpattern}}.}
    \item{\code{getMatchCount}}{Gets number of matches from a
      \code{"\linkS4class{RLBigData}"} object.}
    \item{\code{getNACount}}{Gets number of pairs with unknown matching status
      from a \code{"\linkS4class{RLBigData}"} object.}
    \item{\code{getDbFile}}{Returns path of database file for a data or result
      object.}
    \item{\code{getColumnNames}}{Returns column names of a
      \code{"\linkS4class{RLBigData}"} object as a character vector.}
    \item{\code{getThresholds}}{Calculates upper and lower classification
      based on error bounds.}
}

}
\author{
  Andreas Borg, Murat Sariyar
}

\keyword{internal}
