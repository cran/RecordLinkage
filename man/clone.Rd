\name{clone}
\alias{clone}
\alias{saveRLObject}
\alias{loadRLObject}
\alias{clone-methods}
\alias{clone,RLBigData-method}
\alias{clone,RLResult-method}
\alias{saveRLObject-methods}
\alias{saveRLObject,RLBigData-method}
\alias{saveRLObject,RLResult-method}

\title{
  Serialization of record linkage object.
}
\description{
  Saving, loading and deep copying of record linkage objects
  for big data sets.
}
\usage{
  clone(object, ...)
  saveRLObject(object, file = getDbFile(object), ...)
  loadRLObject(file, inPlace = FALSE)
}
\arguments{
  \item{object}{Object of class \code{"\linkS4class{RLBigData}"}. The object to save.}
  \item{file}{The name of the file to save to or load from.}
  \item{inPlace}{Logical. Wether to use the exisiting file as working copy.
    See details.}
  \item{\dots}{Optional arguments for possible additions, currently not used.}
}
\details{
  As the classes for record linkage of large data sets have a SQLite database
  attached which holds data and generates comparison patterns, a deep copy
  of an instance is not possible by the standard R mechanisms. Also, saving
  and reloading in a different R session makes an object unusable because the
  database connection is lost. These issues are overcome by these
  functions.
  
  \code{clone} makes a deep copy of an object by duplicating the underlying
  database.
  
  \code{saveRLObject} saves an object to a SQLite database file containing
    a representation of the R object as well as the associated database
    tables. By default the current working copy is used.

  \code{loadRLObject} loads an object from a file saved by \code{saveRLObject}.
  
  \code{clone} and \code{saveRLObject} are generic functions with methods for
  \code{"\linkS4class{RLBigData}"} and \code{"\linkS4class{RLResult}"}.

  If \code{loadRLObject} is called with \code{inPlace = FALSE} (the default),
  a working copy of the database is made in a temporary file and the original
  file left untouched. Calling with \code{inPlace = TRUE} sets the provided file as
  working copy and changes made to the database are persistent. This option is
  useful when working with large files in order to prevent disk usage
  overhead.


  
}
\value{
  \code{clone} returns a deep copy of \code{object}.
    
  \code{loadRLObject} returns the loaded object.

  \code{saveRLObject} is used for its side effects.
}

\section{Side effects}{
  \code{clone} creates a new temporary SQLite database (a copy of the one
    belonging to \code{object}).

  \code{saveRLObject} writes to the specified file.

  \code{loadRLObject} creates a new temporary SQLite database in the case
  \code{inPlace = FALSE}.
}

\note{
  Objects loaded with \code{inPlace = TRUE} should still be saved with
  \code{saveRLObject}, as the R object might be altered.

  The constructors \code{\link[=RLBigDataDedup]{RLBigData*}} create database
  files in a temporary directory by default. Be careful to save to a persistent
  directory when objects should be loaded in a different R session.
}

\seealso{\link{clone-methods}, \link{saveRLObject-methods}}


\author{
  Andreas Borg, Murat Sariyar
}


\keyword{classif}
\keyword{file}