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
\alias{getNonMatchCount}
\alias{getNonMatchCount-methods}
\alias{getNonMatchCount,RLBigData-method}
\alias{getNACount}
\alias{getNACount-methods}
\alias{getNACount,RLBigData-method}
\alias{getColumnNames}
\alias{getColumnNames-methods}
\alias{getColumnNames,RLBigDataDedup-method}
\alias{getColumnNames,RLBigDataLinkage-method}
\alias{getThresholds}
\alias{countpattern}
\alias{hasWeights}
\alias{hasWeights-methods}
\alias{hasWeights,RecLinkData-method}
\alias{hasWeights,RLBigData-method}


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
  getSQLStatement(data1, data2 = data1, con, type, blockFld, excludeFld,
    strcmpFld, strcmpFun, phoneticFld, phoneticFun)
  init_sqlite_extensions(db)
  .allows_extensions(db)
  .lib_path()
  getPatternCounts(x, n=10000, cutoff=1, withProgressBar = (sink.number()==0))
  getMatchCount(object)
  getNonMatchCount(object)
  getNACount(object)
  getColumnNames(object, withExcluded = FALSE)
  getThresholds(W, M, U, my, ny)
  countpattern(x, matching = FALSE)
  hasWeights(object)
}

\arguments{
  \item{x, object}{Depends on the function, usually a \code{"\linkS4class{RecLinkData}"},
    \code{"\linkS4class{RLBigData}"} or \code{"\linkS4class{RLResult}"}.
    A matrix of binary observations for \code{countpattern}.}
  \item{n}{Maximum number of comparison patterns to fetch.}
  \item{blockfld}{Blocking definition as in
    \code{\link[=RLBigDataDedup]{RLBigData*}}.}
  \item{phoneticFld}{Indices of attributes for which phonetic code should be used.}
  \item{phoneticFun}{Name of phonetic function as in
    \code{\link[=RLBigDataDedup]{RLBigData*}}.}
  \item{data1, data2}{Data frame with records.}
  \item{type}{"linkage" or "deduplication", the type of linkage.}
  \item{excludeFld}{Indices or names of fields to exclude from comparison.}
  \item{strcmpFld}{Indices of attributes for which string comparison should be used.}
  \item{strcmpFun}{Name of string comparison function as in
    \code{\link[=RLBigDataDedup]{RLBigData*}}.}
  \item{con}{A SQLite database connection.}
  \item{coln}{Column names of records.}
  \item{db}{Database connection.}
  \item{cutoff}{Threshold from which string comparison values are rounded to 1.}
  \item{withProgressBar}{Whether to display a progress bar.}
  \item{withExcluded}{Logical. Wether to output names of excluded columns}
  \item{W,M,U}{Linkage weights, m- and u-probabilites. See \code{\link{emWeights}}
    for details.}
  \item{W,M,U}{Error bounds. See \code{\link{emClassify}} for details.}
  \item{\dots}{Optional arguments.}
  \item{matching}{If TRUE an additional vector is returned which stores which
          row belongs to which pattern.}
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
    \item{\code{countpattern}}{Modified version of \code{\link[e1071]{countpattern}}
      in package e1071.}
    \item{\code{hasWeights}}{Determines whether a data object has weights.}
}

}
\author{
  Andreas Borg, Murat Sariyar
}

\keyword{internal}
