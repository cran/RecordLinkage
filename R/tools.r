# tools.r: various utility functions

# estimate the number of record pairs
setGeneric(
  name = "getExpectedSize",
  def = function(object, ...) standardGeneric("getExpectedSize")
)

setMethod(
  f = "getExpectedSize",
  signature = "data.frame",
  definition = function(object, blockfld=list())
  {
    if(!is.list(blockfld)) blockfld = list(blockfld)
    rpairs <- RLBigDataDedup(object)
    nData <- nrow(object)
    nAll <- nData * (nData - 1) / 2
    if (length(blockfld)==0) return(nAll)
    coln <- make.db.names(rpairs@con, colnames(object))

    # ergibt Wahrscheinlichkeit, dass mit gegebenen Blockingfeldern
    # ein Paar nicht gezogen wird
    blockelemFun <- function(blockelem)
    {
      if(is.character(blockelem)) blockelem <- match(blockelem, colnames(object))
      freq <- dbGetQuery(rpairs@con,
        sprintf("select count(*) as c from data group by %s having c > 1 and %s",
          paste("\"", coln[blockelem], "\"", sep="", collapse=", "),
          paste(
            sapply(coln[blockelem], sprintf, fmt = "\"%s\" is not null"),
            collapse = " and "
          )
        )
      )
      1 - (sum(sapply(freq,  function(x) x * (x-1) /2)) / nAll)
    }
    res <- nAll * (1-prod(sapply(blockfld, blockelemFun)))

    # avoid clutter from temporary files
    dbDisconnect(rpairs@con)
    unlink(rpairs@dbFile)

    round(res)
  }
)

setMethod(
  f = "getExpectedSize",
  signature = "RLBigDataDedup",
  definition = function(object)
  {
    blockfld <- object@blockFld
    if(!is.list(blockfld)) blockfld <- list(blockfld)
    nData <- nrow(object@data)
    nAll <- nData * (nData - 1) / 2
    if (length(blockfld)==0) return(nAll)
    coln <- make.db.names(object@con, colnames(object@data))

    # ergibt Wahrscheinlichkeit, dass mit gegebenen Blockingfeldern
    # ein Paar nicht gezogen wird
    blockelemFun <- function(blockelem)
    {
      if(is.character(blockelem)) blockelem <- match(blockelem, colnames(object@data))
      freq <- dbGetQuery(object@con,
        sprintf("select count(*) as c from data group by %s having c > 1 and %s",
          paste("\"", coln[blockelem], "\"", sep="", collapse=", "),
          paste(
            sapply(coln[blockelem], sprintf, fmt = "\"%s\" is not null"),
            collapse = " and "
          )
        )
      )
      1 - (sum(sapply(freq,  function(x) x * (x-1) /2)) / nAll)
    }
    res <- nAll * (1-prod(sapply(blockfld, blockelemFun)))


    res
  }
)

setMethod(
  f = "getExpectedSize",
  signature = "RLBigDataLinkage",
  definition = function(object)
  {
    blockfld <- object@blockFld
    if(!is.list(blockfld)) blockfld <- list(blockfld)
    nData1 <- nrow(object@data1)
    nData2 <- nrow(object@data2)
    nAll <- nData1 * nData2
    if (length(blockfld)==0) return(nAll)
    coln <- make.db.names(object@con, colnames(object@data1))

    # ergibt Wahrscheinlichkeit, dass mit gegebenen Blockingfeldern
    # ein Paar nicht gezogen wird
    blockelemFun <- function(blockelem)
    {
      if(is.character(blockelem)) blockelem <- match(blockelem, colnames(object@data))
      freq <- dbGetQuery(object@con,
        sprintf("select count(*) as c from data1 t1, data2 t2 where %s",
          paste(
            sapply(coln[blockelem], sprintf, fmt = "t1.\"%1$s\"=t2.\"%1$s\""),
            collapse = " and "
          )
        )
      )$c
      1 - (freq / nAll)
    }
    res <- nAll * (1-prod(sapply(blockfld, blockelemFun)))
    res
  }
)



# Subscript operator for RecLinkData and RecLinkResult objects
"[.RecLinkData" <- function(x,i)
{
  ret <- x
  ret$pairs <- x$pairs[i,]
  ret$Wdata <- x$Wdata[i]
  ret
}

"[.RecLinkResult" <- function(x,i)
{
  ret <- "[.RecLinkData"(x, i)
  ret$prediction <- x$prediction[i]
  ret
}

# append two data sets into one

setGeneric(
  name = "%append%",
  def = function(x, y) standardGeneric("%append%")
)

setMethod(
  f = "%append%",
  signature = c("RecLinkData", "RecLinkData"),
  definition = function(x, y)
  {
    ret <- x
    ret$pairs <- rbind(x$pairs, y$pairs)
    # look if weights are stored in both, one or none of the objects
    withoutWeights <- is.null(x$Wdata) + is.null(y$Wdata)
    if(withoutWeights==1) warning("Weights in one of the objects only!")
    if(withoutWeights==0) ret$Wdata <- c(x$Wdata, y$Wdata)
    ret
  }
)


setMethod(
  f = "%append%",
  signature = c("RecLinkResult", "RecLinkResult"),
  definition = function(x, y)
  {
    ret <- selectMethod("%append%", c("RecLinkData", "RecLinkData"))(x, y)
    ret$prediction <- c(x$prediction, y$prediction)
    class(ret$prediction) <- "factor"
    levels(ret$prediction) <- c("N", "P", "L")
    ret
  }
)
