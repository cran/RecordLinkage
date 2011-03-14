# internal variables to store supported string comparison and phonetic functions

.supportedStrcmp <- c("jarowinkler", "levenshtein")
.supportedPhonetic <- c("pho_h", "soundex") # soundex directly by sqlite


#' Abstract class for large datasets
#'
#' Base class for large datasets (millions of record pairs). Each object holds
#' a database connection to store records and perform the generation of
#' comparison patterns. The database resides in a temporary RSQLite file
#' which is deleted on exit.
#'
#' @slot frequencies Relative frequency of attribute values
#' @slot blockFld Blocking definition
#' @slot excludeFld Indices of attributes to exclude from comparison
#' @slot drv Database driver. Points to a single SQLite instance
#' @slot con Database connection. Exclusive to one object
setClass(
  Class = "RLBigData",
  representation = representation(
    frequencies = "numeric",
    blockFld = "list",
    excludeFld = "numeric",
    strcmpFld = "numeric",
    strcmpFun = "character",
    phoneticFld = "numeric",
    phoneticFun ="character",
    drv = "DBIDriver",
    con = "DBIConnection",
    dbFile = "character",
    "VIRTUAL"
  )
)    

#' Large deduplication data set
#'
#' Realization of RLBigData for deduplication of a single data set. Records are
#' stored as rows in \code{data}. Two records \code{data[i,]} and {data[j,]} are 
#' considered equal if and only if \code{identity[i]==identity[j]}
#'
#' @slot data Records to deduplicate
#' @slot identity Identity vector. 
setClass(
  Class = "RLBigDataDedup",
  contains = "RLBigData",
  representation = representation(
    data = "data.frame",
    identity = "factor"
  )
)    
#' Large linkage data set
#'
#' Realization of RLBigData for linkage of two data sets. Records are
#' stored as rows in \code{data1} and \code{data2}. Two records \code{data1[i,]} 
#' and {data2[j,]} are considered equal if and only if 
#' \code{identity1[i]==identity2[j]}
#'
#' @slot data Records to deduplicate
#' @slot identity Identity vector. 
setClass(
  Class = "RLBigDataLinkage",
  contains = "RLBigData",
  representation = representation(
    data1 = "data.frame",
    data2 = "data.frame",
    identity1 = "factor",
    identity2 = "factor"
  )
)    

# constructor
RLBigDataDedup <- function(dataset, identity = NA, blockfld = list(), 
  exclude = numeric(0), strcmp = numeric(0), 
  strcmpfun = "jarowinkler", phonetic=numeric(0), phonfun = "pho_h")
{
  # error checking
  if (!is.data.frame(dataset) && !is.matrix(dataset))
    stop("dataset must be a matrix or data frame!") 
  nfields <- ncol(dataset)

  # if strings are used to identify columns, convert to numeric indices
  if (is.character(exclude)) exclude <- match(exclude, colnames(dataset))
  if (is.character(strcmp)) strcmp <- match(strcmp, colnames(dataset))
  if (is.character(phonetic)) phonetic <- match(phonetic, colnames(dataset))

  if (any(exclude <=0 | exclude > nfields)) stop ("exclude contains out-of-bounds value!")  
  if (any(strcmp <=0 | strcmp > nfields)) stop ("strcmp contains out-of-bounds value!")  
  if (any(phonetic <=0 | phonetic > nfields)) stop ("phonetic contains out-of-bounds value!")  

  if (is.list(identity)) stop("identity must not be a list!")
  if (!missing(identity) && nrow(dataset) != length(identity))
    stop("length of identity differs from number of records!")

  # if strcmp or phonetic is TRUE, set it to all existing columns
  # excluded fields are omitted during construction of SQL commands
  if (isTRUE(strcmp)) strcmp = 1:nfields
  if (isTRUE(phonetic)) phonetic = 1:nfields

  # at this point, strcmp, exclude and phonetic should be numeric vectors
  if (!is.numeric(strcmp)) stop("strcmp has wrong type!")
  if (!is.numeric(phonetic)) stop("strcmp has wrong type!")
  if (!is.numeric(exclude)) stop("exclude has wrong type!")

  # issue a warning if both string metric and phonetic code is used on one field
    if (length(intersect(phonetic,strcmp)) > 0)
        warning("Both phonetics and string metric are used on some fields!")
        
  # check if string comparison / phonetic function is supported and
  # has the correct format
  if (!is.character(strcmpfun)) stop(paste("Wrong type of strcmpfun:", class(strcmpfun)))
  if (!is.character(phonfun)) stop(paste("Wrong type of phonfun:", class(phonfun)))
  if (length(strcmpfun) > 1) stop("strcmpfun must have length 1!")
  if (length(phonfun) > 1) stop("phonfun must have length 1!")
  if (!(strcmpfun %in% .supportedStrcmp))
    stop ("unkown string comparison function!")
  if (!(phonfun %in% .supportedPhonetic))
    stop ("unkown phonetic function!")

  # put blockfld into list if necessary, check format, 
  # convert string indices to numeric indices
  if (!is.list(blockfld) && !is.null(blockfld)) blockfld <- list(blockfld)
  if (!all(sapply(blockfld, function(x) class(x) %in% c("character", "integer", "numeric"))))
    stop("blockfld has wrong format!")
  blockfld <- lapply(blockfld, 
   function(x) {if (is.character(x)) match(x, colnames(dataset)) else (x)})
  if(any(unlist(blockfld) <= 0 | unlist(blockfld) > nfields))
    stop("blockfld countains out-of-bounds value!")
    
  # cast dataset to data.frame
  # also constructs column names
  dataset <- as.data.frame(dataset)

  # construct column names if not assigned 
#  if (is.null(names(dataset)))
#    names(dataset) <- paste("V", 1:nfields, sep="")

  # set up database
  tmpfile <- tempfile()
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname=tmpfile)
  coln <- make.db.names(con,colnames(dataset))

  # construct object  
  object <- new("RLBigDataDedup", data=dataset, identity=factor(identity),
    blockFld = blockfld, excludeFld = exclude, strcmpFld = strcmp,
    strcmpFun = strcmpfun, phoneticFld = phonetic, phoneticFun = phonfun,
    drv = drv, con = con, frequencies = sapply(dataset, function(x) 1/length(unique(x))),
    dbFile = tmpfile
  )

  # write records to database
  dbWriteTable(con, "data", data.frame(row_names = 1:nrow(dataset), dataset, identity = identity),
    row.names=FALSE)

  # create indices to speed up blocking
  for (blockelem in blockfld)
  {
    # include names in '' because they might be keywords
    query <- sprintf("create index 'index_%s' on data (%s)",
     paste(coln[blockelem], collapse="_"),
     paste("'", coln[blockelem], "'", sep="", collapse=", "))
    dbGetQuery(con, query)
  }
  # create index on identity vector to speed up identifying true matches
  dbGetQuery(con, "create index index_identity on data (identity)")

  # create index on id to speed up join operations (in getTable)
  dbGetQuery(con, "create index index_data_id on data (row_names)")

  # init extension functions (string comparison, phonetic code) for SQLite
  init_sqlite_extensions(con)
  
  # check that set of generated pairs is not empty
  begin(object)
  res <- nextPairs(object, 1)
  clear(object)
  if(nrow(res)==0) stop("No pairs generated. Check blocking criteria.")

  return(object)
}

# constructor for RLBigDataLinkage (linking two datasets)
RLBigDataLinkage <- function(dataset1, dataset2, identity1 = NA, 
  identity2 = NA, blockfld = list(), exclude = numeric(0), strcmp = numeric(0), 
  strcmpfun = "jarowinkler", phonetic=numeric(0), phonfun = "pho_h")
{
  if (!is.data.frame(dataset1) && !is.matrix(dataset1))
    stop("dataset1 must be a matrix or data frame!") 
  nfields <- ncol(dataset1)

  if (!is.data.frame(dataset2) && !is.matrix(dataset2))
    stop("dataset2 must be a matrix or data frame!") 

  if (nfields != ncol(dataset2))
    stop("dataset1 and dataset2 have different numbers of columns!")

  # if strings are used to identify columns, convert to numeric indices
  if (is.character(exclude)) exclude <- match(exclude, colnames(dataset1))
  if (is.character(strcmp)) strcmp <- match(strcmp, colnames(dataset1))
  if (is.character(phonetic)) phonetic <- match(phonetic, colnames(dataset1))

  if (any(exclude <=0 | exclude > nfields)) stop ("exclude contains out-of-bounds value!")  
  if (any(strcmp <=0 | strcmp > nfields)) stop ("strcmp contains out-of-bounds value!")  
  if (any(phonetic <=0 | phonetic > nfields)) stop ("phonetic contains out-of-bounds value!")  

  if (is.list(identity1)) stop("identity1 must not be a list!")
  if (!missing(identity1) && nrow(dataset1) != length(identity1))
    stop("length of identity1 differs from number of records!")

  if (is.list(identity2)) stop("identity2 must not be a list!")
  if (!missing(identity2) && nrow(dataset2) != length(identity2))
    stop("length of identity2 differs from number of records!")

  # if strcmp or phonetic is TRUE, set it to all existing columns
  # excluded fields are omitted during construction of SQL commands
  if (isTRUE(strcmp)) strcmp = 1:nfields
  if (isTRUE(phonetic)) phonetic = 1:nfields

  # at this point, strcmp, exclude and phonetic should be numeric vectors
  if (!is.numeric(strcmp)) stop("strcmp has wrong type!")
  if (!is.numeric(phonetic)) stop("strcmp has wrong type!")
  if (!is.numeric(exclude)) stop("exclude has wrong type!")

  # issue a warning if both string metric and phonetic code is used on one field
    if (length(intersect(phonetic,strcmp))>0)
        warning("Both phonetics and string metric are used on some fields!")
        
  # check if string comparison / phonetic function is supported and
  # has the correct format
  if (!is.character(strcmpfun)) stop(paste("Wrong type of strcmpfun:", class(strcmpfun)))
  if (!is.character(phonfun)) stop(paste("Wrong type of phonfun:", class(phonfun)))
  if (length(strcmpfun) > 1) stop("strcmpfun must have length 1!")
  if (length(phonfun) > 1) stop("phonfun must have length 1!")
  if (!(strcmpfun %in% .supportedStrcmp))
    stop ("unkown string comparison function!")
  if (!(phonfun %in% .supportedPhonetic))
    stop ("unkown phonetic function!")

  # put blockfld into list if necessary, check format, 
  # convert string indices to numeric indices
  if (!is.list(blockfld) && !is.null(blockfld)) blockfld <- list(blockfld)
  if (!all(sapply(blockfld, function(x) class(x) %in% c("character", "integer", "numeric"))))
    stop("blockfld has wrong format!")
  blockfld <- lapply(blockfld, 
   function(x) {if (is.character(x)) match(x, colnames(dataset1)) else (x)})
  if(any(unlist(blockfld) <= 0 | unlist(blockfld) > nfields))
    stop("blockfld countains out-of-bounds value!")

  # cast data sets to data.frame
  # also constructs missing column names
  dataset1 <- as.data.frame(dataset1)
  dataset2 <- as.data.frame(dataset2)


  # enforce same column names for dataset2
  names(dataset2) <- names(dataset1)

  # set up database
  tmpfile <- tempfile()
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname=tmpfile)
  coln <- make.db.names(con,colnames(dataset1))

  # convert identity to factors (so that only level indices are used in the
  # database), enforce same levels
  if (class(identity1) != class(identity2))
    warning("identity1 and identity2 have different types!")
  identLevels <- as.character(unique(c(identity1, identity2)))
  identity1 <- factor(identity1, levels = identLevels)
  identity2 <- factor(identity2, levels = identLevels)
  
  # calculate frequencies
  # construct object  
  frequencies = sapply(rbind(dataset1, dataset2),
     function(x) 1/length(unique(x)))
  object <- new("RLBigDataLinkage", data1=as.data.frame(dataset1), 
    data2 = as.data.frame(dataset2), identity1 = identity1,
    identity2 = identity2, blockFld = blockfld, 
    excludeFld = exclude, strcmpFld = strcmp, strcmpFun = strcmpfun, 
    phoneticFld = phonetic, phoneticFun = phonfun, drv = drv, con = con, 
    frequencies = frequencies, dbFile = tmpfile
  ) 

  # write records to database
  dbWriteTable(con, "data1", data.frame(row_names = 1:nrow(dataset1), dataset1,
    identity = identity1), row.names=FALSE)
  dbWriteTable(con, "data2", data.frame(row_names = 1:nrow(dataset2), dataset2,
    identity = identity2), row.names=FALSE)
#  dbWriteTable(con, "data1", data.frame(dataset1, identity = identity1))
#  dbWriteTable(con, "data2", data.frame(dataset2, identity = identity2))
#
  # create indices to speed up blocking
  for (tablename in c("data1", "data2"))
  {
    for (blockelem in blockfld)
    {
      query <- sprintf("create index 'index_%s_%s' on '%s' (%s)",
       tablename,
       paste(coln[blockelem], collapse="_"),
       tablename,
      paste("'", coln[blockelem], "'", sep="", collapse=", "))
      dbGetQuery(con, query)
    }
    # create index on identity vector to speed up identifying true matches
    dbGetQuery(con, sprintf("create index index_identity_%s on %s (identity)",
        tablename, tablename))
  }

  # create index on id to speed up join operations (in getTable)
  dbGetQuery(con, "create index index_data1_id on data1 (row_names)")
  dbGetQuery(con, "create index index_data2_id on data2 (row_names)")

  # init extension functions (string comparison, phonetic code) for SQLite
  init_sqlite_extensions(con)
  
  # check that set of generated pairs is not empty
  begin(object)
  res <- nextPairs(object, 1)
  clear(object)
  if(nrow(res)==0) stop("No pairs generated. Check blocking criteria.")

  return(object)
}