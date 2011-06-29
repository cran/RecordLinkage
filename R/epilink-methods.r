# epilink-methods.r: Functions for the Epilink matching procedure
# See Continiero et al.: The EpiLink record linkage software, in:
# Methods of Information in Medicine 2005, 44(1):66-71.

setGeneric(
  name = "epiClassify",
  def = function(rpairs, threshold.upper, threshold.lower=threshold.upper, ...)
    standardGeneric("epiClassify")
)

setMethod(
  f = "epiClassify",
  signature = "RecLinkData",
  definition = function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper)
  {    
  
    if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
      stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))
  
    if (nrow(rpairs$pairs) == 0)
      stop("No record pairs!")
  
    if (is.null(rpairs$Wdata))
      stop("No weights in rpairs!")
  
    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))
  
    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))
  
    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
      threshold.upper, threshold.lower))
      
    prediction=rep("P",nrow(rpairs$pairs))
    prediction[rpairs$Wdata>=threshold.upper]="L"
    prediction[rpairs$Wdata<threshold.lower]="N"
    
    ret=rpairs # keeps all components of rpairs
    ret$prediction=factor(prediction,levels=c("N","P","L"))
    ret$threshold=threshold.upper
    class(ret)="RecLinkResult"
    return(ret)
  }
) # end of setMethod

setMethod(
  f = "epiClassify",
  signature = "RLBigData",
  definition = function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper, e=0.01, 
                        f=getFrequencies(rpairs), withProgressBar = (sink.number()==0))
  {    
    if (!is.numeric(threshold.upper))
      stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))
  
    if (!is.numeric(threshold.lower))
      stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))
  
    if (threshold.upper < threshold.lower)
      stop(sprintf("Upper threshold %g lower than lower threshold %g",
      threshold.upper, threshold.lower))

    if(!isIdCurrent(rpairs@con)) stop(paste("Invalid SQLite connection in rpairs!",
      "See '?saveRLObject' on how to make persistant copies of such objects."))

    if (dbExistsTable(rpairs@con, "Wdata"))
    {
      query <- "select id1, id2 from Wdata where W >= :upper"
      links <- dbGetPreparedQuery(rpairs@con, query, data.frame(upper = threshold.upper))
      query <- "select id1, id2 from Wdata where W < :upper and W >= :lower"
      possibleLinks <- dbGetPreparedQuery(rpairs@con, query,
        data.frame(upper = threshold.upper, lower = threshold.lower))
      nPairs <- dbGetQuery(rpairs@con, "select count(*) as c from Wdata")$c
    } else
    {

      if (withProgressBar)
      {
        expPairs <- getExpectedSize(rpairs)
        pgb <- txtProgressBar(max=expPairs)
      }
      nPairs <- 0
      n <- 10000
      links <- matrix(nrow=0, ncol=2)
      possibleLinks <- matrix(nrow=0, ncol=2)
      on.exit(clear(rpairs))
      rpairs <- begin(rpairs)

      # weight computation
      if (class(rpairs)=="RLBigDataDedup")
        nAttr <- ncol(rpairs@data) - length(rpairs@excludeFld)
      else # RLBigDataLinkage
        nAttr <- ncol(rpairs@data1) - length(rpairs@excludeFld)

      w=log((1-e)/f, base=2)
      w <- w + numeric(nAttr)
      sumW <- sum(w)
      while(nrow(slice <- nextPairs(rpairs, n)) > 0)
      {
        flush.console()
        ids <- slice[,1:2]
        slice <- t(slice[,-c(1,2,ncol(slice))])
        slice[is.na(slice)] <- 0
        S <- colSums(slice * w)/sumW
        if (any(is.na(S) | S < 0 | S > 1))
          warning("Some weights have illegal values. Check error rate and frequencies!")
        links <- rbind(links, as.matrix(ids[S >= threshold.upper,]))
        possibleLinks <- rbind(possibleLinks,
          as.matrix(ids[S >= threshold.lower & S < threshold.upper,]))
        nPairs <- nPairs + nrow(ids)
        if (withProgressBar)
        {
          setTxtProgressBar(pgb, nPairs)
          flush.console()
        }
      }
      if (withProgressBar) close(pgb)
    }
    
    new("RLResult", data = rpairs, links = as.matrix(links),
      possibleLinks = as.matrix(possibleLinks),
      nPairs = nPairs)
  }
) # end of setMethod


setGeneric(
  name = "epiWeights",
  def = function(rpairs, e=0.01, f=getFrequencies(rpairs), ...)
    standardGeneric("epiWeights")
)

setMethod(
  f = "epiWeights",
  signature = "RecLinkData",
  definition = function (rpairs, e=0.01, f=rpairs$frequencies)
  {
    # check for erronous input

    if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
      stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

    if (nrow(rpairs$pairs) == 0)
      stop("No record pairs!")

    if (!is.numeric(f))
      stop(sprintf("Illegal type for f: %s", class(f)))

    if (any(f <=0 | f > 1))
      stop(sprintf("Illegal value for f: %s!", paste(f, collapse=", ")))

    if (!is.numeric(e))
      stop(sprintf("Illegal type for e: %s", class(f)))

    if (any(e <0 | e >= 1))
      stop(sprintf("Illegal value for e: %s!", paste(e, collapse=", ")))

    # check condition e <= 1-f, otherwise illegal weights can occur
    if(any(e > 1-f))
      stop("Condition e <= 1-f does not hold, adjust error rate!")

    # leave out ids and matching status
    # weights are computed on transposed matrix of comparison patterns
    # (each column is an observation) to allow vectorization
    pairs=t(as.matrix(rpairs$pairs[,-c(1,2,ncol(rpairs$pairs))]))
    pairs[is.na(pairs)]=0
    w=log((1-e)/f, base=2)

    # recycle w (in order to get the sum of attribute weights right)
    w <- numeric(nrow(pairs)) + w

    # multiply each pattern with the epilink weights
    # the weight for each pattern is the sum of its attribute weights
    S <- colSums(pairs * w)/sum(w)

    if (any(is.na(S) | S < 0 | S > 1))
      warning("Some weights have illegal values. Check error rate and frequencies!")
    rpairs$Wdata=S
    return(rpairs)
  }
)

setMethod(
  f = "epiWeights",
  signature = c("RLBigData", "ANY", "ANY"),
  definition = function (rpairs, e=0.01, f=getFrequencies(rpairs),
      withProgressBar = (sink.number()==0))
  {


    if(!isIdCurrent(rpairs@con)) stop(paste("Invalid SQLite connection in rpairs!",
      "See '?saveRLObject' on how to make persistant copies of such objects."))

    # Delete old weights if they exist
    # vacuum to keep file compact
    dbGetQuery(rpairs@con, "drop table if exists Wdata")
    dbGetQuery(rpairs@con, "vacuum")

    # Create a copy of the record pairs from which comparison patterns will
    # be generated. This allows concurrent writing of calculated weights.
    rpairs_copy <- clone(rpairs)
 #    dbFile2 <- tempfile(tmpdir=dirname(path.expand(rpairs@dbFile)))
#    con2 <- dbConnect(rpairs@drv, dbname = dbFile2)
#    dbGetQuery(con2, "pragma journal_mode=memory")
    # create table where weights are stored


   dbBeginTransaction(rpairs@con)

    dbGetQuery(rpairs@con, "create table Wdata (id1 integer, id2 integer, W real)")


    if (withProgressBar)
    {
      expPairs <- getExpectedSize(rpairs_copy)
      pgb <- txtProgressBar(max=expPairs)
    }

    rpairs_copy <- begin(rpairs_copy)
    nPairs <- 0
    n <- 10000
    i = n



    if (class(rpairs_copy)=="RLBigDataDedup")
      nAttr <- ncol(rpairs_copy@data) - length(rpairs_copy@excludeFld)
    else # RLBigDataLinkage
      nAttr <- ncol(rpairs_copy@data1) - length(rpairs_copy@excludeFld)

    w=log((1-e)/f, base=2)
    w <- w + numeric(nAttr)
    sumW <- sum(w)
      # weight computation

    row_sum <- function(r,w)
    {
      return(sum(r*w,na.rm=TRUE))
    }

    while(nrow(slice <- nextPairs(rpairs_copy, n)) > 0)
    {
      ids <- slice[,1:2]
      slice <- t(slice[,-c(1,2,ncol(slice))])
        slice[is.na(slice)] <- 0
        #
      S <- colSums(slice * w)/sumW

      if (any(is.na(S) | S < 0 | S > 1))
        warning("Some weights have illegal values. Check error rate and frequencies!")


      dbGetPreparedQuery(rpairs@con, "insert into Wdata values (?, ?, ?)",
        cbind(ids, S))

      nPairs <- nPairs + length(S)
      if (withProgressBar)
      {
        setTxtProgressBar(pgb, nPairs)
        flush.console()
      }
    }
    if (withProgressBar) close(pgb)

    # create index, this speeds up the join operation of getPairs
    # significantly
    dbGetQuery(rpairs@con, "create index index_Wdata_id on Wdata (id1, id2)")
    dbGetQuery(rpairs@con, "create index index_Wdata_W on Wdata (W)")

    dbCommit(rpairs@con)

    # remove copied database
    clear(rpairs_copy)
    dbDisconnect(rpairs_copy@con)
    unlink(rpairs_copy@dbFile)

    rpairs
  }
) # end of setMethod
