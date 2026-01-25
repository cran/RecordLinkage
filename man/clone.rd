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
  saveRLObject(object, file, ...)
  loadRLObject(file)
}
\arguments{
  \item{object}{Object of class \code{"\linkS4class{RLBigData}"}. The object to save.}
  \item{file}{The name of the file to save to or load from.}
  \item{\dots}{Optional arguments for possible additions, currently not used.}
}
\details{

  The classes for big data sets make use of file-backed data structures from
  the \pkg{ff} package, therefore the \code{load} and \code{save} mechanism of
  R is not sufficient for persistent storage of these objects. Also, assignment
  via \code{<-} does not duplicate the \pkg{ff} data structures.

  \code{clone} makes a deep copy of an object by duplicating the underlying
  files.
  
  \code{saveRLObject} saves an object to zip file containing
    a dump of the R object as well as the associated \pkg{ff} files.

  \code{loadRLObject} loads an object from a file saved by \code{saveRLObject}.
  
  \code{clone} and \code{saveRLObject} are generic functions with methods for
  \code{"\linkS4class{RLBigData}"} and \code{"\linkS4class{RLResult}"}.

  If \code{loadRLObject} is called with \code{inPlace = FALSE} (the default),
  a working copy of the database is made in a temporary file and the original
  file left untouched. Calling with \code{inPlace = TRUE} sets the provided file as
  working copy and changes made to the database are persistent. This option is
  useful when working with large files in order to prevent disk usage
  overhead.
  
  \code{saveRLObject} and \code{loadRLObject} require working zip / unzip programs.

  
}
\value{
  \code{clone} returns a deep copy of \code{object}.
    
  \code{loadRLObject} returns the loaded object.

  \code{saveRLObject} is used for its side effects.
}


\note{
  Objects loaded with \code{inPlace = TRUE} must be saved again after changes
  have been made to the object (e.g. calculation of weights).
}


\author{
  Andreas Borg, Murat Sariyar
}


\keyword{file}