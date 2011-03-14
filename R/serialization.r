# functions and methods to clone Record Linkage objects and save them
# to disk (applying to RL* object which store data in a database)

# Clone object, including database.
setGeneric(
  name = "clone",
  def = function(object, ...) standardGeneric("clone")
)

setMethod(
  f = "clone",
  signature = "RLBigData",
  definition = function(object, ...)
  {
    result <- object
    result@dbFile <- tempfile()
    sqliteCopyDatabase(from = object@con, to = result@dbFile)
    result@con <- dbConnect(result@drv, dbname = result@dbFile)
    init_sqlite_extensions(result@con)
    result
  }
)

setMethod(
  f = "clone",
  signature = "RLResult",
  definition = function(object, ...)
  {
  }
)


# Save object to disk. File format is a SQLite database with
# additional table to hold the R object
setGeneric(
  name = "saveRLObject",
  def = function(object, file = getDbFile(object), ...) standardGeneric("saveRLObject")
)

setMethod(
  f = "saveRLObject",
  signature = "RLBigData",
  definition = function(object, file = getDbFile(object), ...)
  {
    # only if file differs from current working copy:
    # first, save existing database and open for the following operations
    if (!missing(file))
    {
      sqliteCopyDatabase(from = object@con, to = file)
      target <- dbConnect(object@drv, file)
    } else target <- object@con
    
    # serialize R object and store in a BLOB
    serializedObj <- serialize(object, connection = NULL)
    dbGetQuery(target, "drop table if exists serialization")
    dbGetQuery(target, "create table serialization (data blob)")
    dbGetPreparedQuery(target, "insert into serialization values (:val)",
      data.frame(val=I(list(serializedObj))))
    # if the target db is newly created, it can be closed
    if (!missing(file)) dbDisconnect(target)
  }
)

setMethod(
  f = "saveRLObject",
  signature = "RLResult",
  definition = function(object, file = getDbFile(object), ...)
  {
  }
)

# Load object from disk.
loadRLObject <- function(file, inPlace = FALSE)
{
  # open connection and verify that a table serializatin exists
  # (this is where the object is saved)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = file)
  if (!dbExistsTable(con, "serialization"))
    stop(sprintf("Could not find serialized R object in database file %s!", file))

  # Load object. The returned data frame holds the serialized object as a
  # raw vector packed in a list
  object <- unserialize(dbReadTable(con, "serialization")[1,1][[1]])
  if (!inherits(object, c("RLBigData", "RLResult")))
    stop(sprintf("Found an object of unsupported class: %s", class(object)))

  # for inPlace = FALSE:
  # copy database to temporary file and delete serialization in db
  if(!inPlace)
  {
    newfile <- tempfile()
    sqliteCopyDatabase(from = con, to = newfile)
    dbDisconnect(con)
    con <- dbConnect(drv, dbname=newfile)
    dbGetQuery(con, "drop table serialization")
  } else
  {
    # otherwise keep the file and do not delete object so that file can
    # be reloaded if closed without calling saveRLObject again
    newfile <- file
  }

  # attach connection info to object
  if(inherits(object, "RLBigData"))
  {
    object@dbFile <- newfile
    object@drv <- drv
    object@con <- con
    init_sqlite_extensions(object@con)
  } else if (inherits(object, "RLResult"))
  {
    object@data@dbFile <- newfile
    object@data@drv <- drv
    object@data@con <- con
    init_sqlite_extensions(object@data@con)
  }
  object
}

setGeneric(
  name = "getDbFile",
  def = function(object) standardGeneric("getDbFile")
)

setMethod(
  f = "getDbFile",
  signature = "RLBigData",
  definition = function(object) object@dbFile
)

setMethod(
  f = "getDbFile",
  signature = "RLResult",
  definition = function(object) object@data@dbFile
)
