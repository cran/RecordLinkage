\name{RLdata}
\docType{data}
\alias{RLdata500}
\alias{RLdata10000}
\alias{identity.RLdata500}
\alias{identity.RLdata10000}

\title{Test data for Record Linkage}

\description{
  These tables contain artificial personal data for the 
  evaluation of Record Linkage procedures. Some records have been duplicated
  with randomly generated errors. \code{RLdata500} contains 50 duplicates,
  \code{RLdata10000} 1000 duplicates.
}

\usage{RLdata500
RLdata10000
identity.RLdata500
identity.RLdata10000}

\format{\code{RLdata500} and \code{RLdata10000} are character matrices with 
  500 and 10000 records. Each row represents one record, with the following
  columns:
  \describe{
    \item{fname_c1}{First name, first component}
    \item{fname_c2}{First name, second component}
    \item{lname_c1}{Last name, first component}
    \item{lname_c2}{Last name, second component}
    \item{by}{Year of birth}
    \item{bm}{Month of birth}
    \item{bd}{Day of birth}
  }
  \code{identity.RLdata500} and \code{identity.RLdata10000} are integer vectors
  representing the true record
  ids of the two data sets. Two records are duplicates, if and only if their
  corresponding values in the identity vector agree.
}

\source{Generated with the data generation component of Febrl (Freely 
  Extensible Biomedical Record Linkage), version 0.3. See 
  \url{http://datamining.anu.edu.au/projects/linkage.html} for details.
  The following data sources were used (all relate to Germany):

  \url{http://blog.beliebte-vornamen.de/2009/02/prozentuale-anteile-2008/}, a
  list of the frequencies of the 20 most popular female names in 2008.
  
  \url{http://www.beliebte-vornamen.de/760-alle_jahre.htm}, a list of the
  100 most popular first names since 1890. The frequencies found in
  the source above were extrapolated to fit this list.
  
  \url{http://www.ahnenforschung-in-stormarn.de/geneal/nachnamen_100.htm}, a list of the
  100 most frequent family names with frequencies.

  Age distribution as of Dec 31st, 2008, statistics of Statistisches
  Bundesamt Deutschland, taken from the GENESIS database
  (\url{https://www-genesis.destatis.de/genesis/online/logon}).  
  
  Web links as of October 2009.
}
\author{Andreas Borg}

\keyword{datasets}
