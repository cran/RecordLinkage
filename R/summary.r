# summary.r: various functions to print summarized information on an object

summary.RecLinkData <- function(object,...)
{
    if (!("RecLinkData" %in% class(object)) &&
        !("RecLinkResult" %in% class(object)))
        stop(sprintf("Wrong type for object: %s!", class(object)))
    if (object$type=="linkage")
    {
        cat("\nLinkage Data Set\n\n")
        cat(sprintf("%d records in data set 1",nrow(object$data1)),"\n")
        cat(sprintf("%d records in data set 2",nrow(object$data2)),"\n")
    }
    else
    {
        cat("\nDeduplication Data Set\n\n")
        cat(sprintf("%d records",nrow(object$data)),"\n")
    }

    cat(sprintf("%d record pairs",nrow(object$pairs)),"\n")
    cat("\n")
    # the expression "length(which(..." is needed to eliminate NAs
	cat(sprintf("%d matches\n",
        length(which(object$pairs$is_match==TRUE))))
    cat(sprintf("%d non-matches\n",
        length(which(object$pairs$is_match==FALSE))))
	cat(sprintf("%d pairs with unknown status\n",
        length(which(is.na(object$pairs$is_match)))))
    cat("\n")

	if (!is.null(object$Wdata))
	{
		cat("\n")
		cat("Weight distribution:\n\n")
		h=hist(object$Wdata,plot=FALSE)
		c=h$counts
		# nehme Gewichtsintervalle als Indizes, um Histogrammansicht zu erhalten
    names(c)=sapply(1:(length(h$breaks)-1),
      function(x) sprintf("(%g,%g]",h$breaks[x],h$breaks[x+1]))
    # erstes Intervall ist auch links geschlossen
    names(c)[1]=sprintf("[%g,%g]", h$breaks[1], h$breaks[2])
		print(c)
	}
	return(invisible(NULL))
}



summary.RecLinkResult <- function (object, ...)
{
    if (!("RecLinkResult" %in% class(object)))
        stop(sprintf("Wrong type for object: %s!", class(object)))

    summary.RecLinkData(object,...)
    crossTable <- table(as.logical(object$pairs$is_match),object$prediction,
          dnn=list("true status","classification"),useNA="ifany")

    cat("\n")
    cat(sprintf("%d links detected", sum(crossTable[,"L"])),"\n")
    cat(sprintf("%d possible links detected", sum(crossTable[,"P"])),"\n")
    cat(sprintf("%d non-links detected", sum(crossTable[,"N"])),"\n")

    cat("\n")

    # print the following summary only if matching status is known at leas
    # for some pairs
    if (nrow(crossTable) >= 2)
    {
      TP=crossTable["TRUE", "L"] # true positive
      FP=crossTable["FALSE", "L"] # false positive
      TN=crossTable["FALSE", "N"] # true negative
      FN=crossTable["TRUE", "N"] # false negative

      alpha=FN/(TP+FN)
      beta=FP/(TN+FP)
      accuracy=(TP+TN)/(TP+TN+FP+FN)
      cat(sprintf("alpha error: %f\n",alpha))
      cat(sprintf("beta error: %f\n",beta))
      cat(sprintf("accuracy: %f\n",accuracy))
      cat("\n\n")
    }
    cat("Classification table:\n\n")
    print(crossTable)
  	return(invisible(NULL))
}

texSummary <- function (object)
{
    library(xtable)
    TP=length(which(object$pairs$is_match & object$prediction=="L")) # true positive
    FP=length(which(!object$pairs$is_match & object$prediction=="L")) # false positive
    TN=length(which(!object$pairs$is_match & object$prediction=="N")) # true negative
    FN=length(which(object$pairs$is_match & object$prediction=="N")) # false negative
    
    alpha=FN/(TP+FN)
    beta=FP/(TN+FP)
    accuracy=(TP+TN)/(TP+TN+FP+FN)

    cat("\\begin{description}\n")
    cat(sprintf("\\item[alpha error] %f\n",alpha))
    cat(sprintf("\\item[beta error] %f\n",beta))
    cat(sprintf("\\item[accuracy] %f\n",accuracy))
    cat("\\end{description}\n")
    cat("\n")
#    cat("Classification table:\n\n")
    print(xtable(table(as.logical(object$pairs$is_match),object$prediction,
          dnn=list("true status","classification"),useNA="ifany")),
          floating=FALSE, latex.environments=NULL)
}


setMethod(
  f = "show",
  signature = "RLBigData",
  definition = function(object)
  {
    # shortcut to database connection
    con <- object@con
    if (class(object)=="RLBigDataDedup")
    {
      cat("\nDeduplication Data Set for large number of data\n\n")
      cat(sprintf("%d records",nrow(object@data)),"\n")
    }

    if (class(object)=="RLBigDataLinkage")
    {
      cat("\nLinkage Data Set for large number of data\n\n")
      cat(sprintf("%d records in first data set",nrow(object@data1)),"\n")
      cat(sprintf("%d records in second data set",nrow(object@data2)),"\n")
    }
  }
)


setMethod(
  f = "summary",
  signature = "RLBigDataDedup",
  definition = function(object)
  {
    val <- list()
    val[["nData"]] <- as.vector(dbGetQuery(object@con, "select count(*) from data")$count)
    val[["attributes"]] <- if (length(rpairs@excludeFld) == 0)
        names(rpairs@data) else names(rpairs@data)[-rpairs@excludeFld]
    val[["blockFld"]] <- lapply(object@blockFld, function(x) names(rpairs@data)[x])
    val$expectedSize <- getExpectedSize(object)
    val$nMatches <- getMatchCount(object)
    # number of unknown pairs: rarely used, calculation may be time-consuming
    #    val$nUnknown <- getNACount(object)
    if(dbExistsTable(object@con, "Wdata"))
    {
  		h=hist(dbReadTable(object@con, "Wdata")$W, plot=FALSE)
  		c=h$counts
  		# nehme Gewichtsintervalle als Indizes, um Histogrammansicht zu erhalten
      names(c)=sapply(1:(length(h$breaks)-1),
        function(x) sprintf("(%g,%g]",h$breaks[x],h$breaks[x+1]))
      # erstes Intervall ist auch links geschlossen
      names(c)[1]=sprintf("[%g,%g]", h$breaks[1], h$breaks[2])
      val$weightHist <- c
    }

    class(val) <- "summaryRLBigDataDedup"
    val
  }
)

print.summaryRLBigDataDedup <- function(x, ...)
{
  cat("RLBigDataDedup, Deduplication object\n")
  cat("\n")
  cat("Number of records:", x$nData, "\n")
  cat("Attributes:", paste(x$attributes, collapse=", "), "\n")
  cat("Blocking definition:", paste(sapply(x$blockFld,
    function(x) paste("[", paste(x, collapse=", "), "]", sep="")), collapse = ", "), "\n")
  cat("Estimated number of record pairs:", x$expectedSize, "\n")
  cat("Number of matches:", x$nMatches, "\n")
# number of unknown pairs: rarely used, calculation may be time-consuming
#  cat("Number of pairs with unknown status:", x$nUnknown, "\n")
  if(!is.null(x$weightHist))
  {
    cat("Weight histogram:\n")
    print(x$weightHist)
  }
}


setMethod(
  f = "summary",
  signature = "RLBigDataLinkage",
  definition = function(object)
  {
    val <- list()
    val[["nData1"]] <- as.vector(dbGetQuery(object@con, "select count(*) from data1")$count)
    val[["nData2"]] <- as.vector(dbGetQuery(object@con, "select count(*) from data2")$count)
    val[["attributes"]] <- if (length(rpairs@excludeFld) == 0)
        names(rpairs@data1) else names(rpairs@data)[-rpairs@excludeFld]
    val[["blockFld"]] <- lapply(object@blockFld, function(x) names(rpairs@data1)[x])
    val$expectedSize <- getExpectedSize(object)
    val$nMatches <- getMatchCount(object)
    # number of unknown pairs: rarely used, calculation may be time-consuming
    #    val$nUnknown <- getNACount(object)
    if(dbExistsTable(object@con, "Wdata"))
    {
  		h=hist(dbReadTable(object@con, "Wdata")$W, plot=FALSE)
  		c=h$counts
  		# nehme Gewichtsintervalle als Indizes, um Histogrammansicht zu erhalten
      names(c)=sapply(1:(length(h$breaks)-1),
        function(x) sprintf("(%g,%g]",h$breaks[x],h$breaks[x+1]))
      # erstes Intervall ist auch links geschlossen
      names(c)[1]=sprintf("[%g,%g]", h$breaks[1], h$breaks[2])
      val$weightHist <- c
    }

    class(val) <- "summaryRLBigDataLinkage"
    val
  }
)

print.summaryRLBigDataLinkage <- function(x, ...)
{
  cat("RLBigDataLinkage, Linkage object\n")
  cat("\n")
  cat("Number of records in dataset 1:", x$nData1, "\n")
  cat("Number of records in dataset 2:", x$nData2, "\n")
  cat("Attributes:", paste(x$attributes, collapse=", "), "\n")
  cat("Blocking definition:", paste(sapply(x$blockFld,
    function(x) paste("[", paste(x, collapse=", "), "]", sep="")), collapse = ", "), "\n")
  cat("Estimated number of record pairs:", x$expectedSize, "\n")
  cat("Number of matches:", x$nMatches, "\n")
# number of unknown pairs: rarely used, calculation may be time-consuming
#  cat("Number of pairs with unknown status:", x$nUnknown, "\n")
  if(!is.null(x$weightHist))
  {
    cat("Weight histogram:\n")
    print(x$weightHist)
  }
}


setMethod(
  f = "summary",
  signature = "RLResult",
  definition = function(object)
  {
    val <- list()
    val[["nPairs"]] <- result@nPairs
    val[["nLinks"]] <- nrow(result@links)
    val[["nPossibleLinks"]] <- nrow(result@possibleLinks)
    class(val) <- "summaryRLResult"
    val
  }
)

print.summaryRLResult <- function(x, ...)
{
  cat("RLBigDataResult object\n")
  cat("\n")
  cat("Number of record pairs:", x$nPairs, "\n")
  cat("Number of detected links:", x$nLinks, "\n")
  cat("Number of detected possible links:", x$nPossibleLinks, "\n")
}


# get accuracy, alpha-error, beta-error etc. from result object
setGeneric(
  name = "getErrorMeasures",
  def = function(object, ...) standardGeneric("getErrorMeasures")
)

# method for 'old' S3 class
setMethod(
  f = "getErrorMeasures",
  signature = "RecLinkResult",
  definition = function(object, ...)
  {
    TP=length(which(object$pairs$is_match & object$prediction=="L")) # true positive
    FP=length(which(!object$pairs$is_match & object$prediction=="L")) # false positive
    TN=length(which(!object$pairs$is_match & object$prediction=="N")) # true negative
    FN=length(which(object$pairs$is_match & object$prediction=="N")) # false negative

    return(list(
      alpha=FN/(TP+FN),
      beta=FP/(TN+FP),
      accuracy=(TP+TN)/(TP+TN+FP+FN),
      precision=TP/(TP+FP),
      sensitivity=TP/(TP+FN),
      specificity=TN/(TN+FP),
      ppv=TP/(TP+FP),
      npv=TN/(TN+FN)
    ))
  }
)

# method for 'new' S4 class (for big data objects)
setMethod(
  f = "getErrorMeasures",
  signature = "RLResult",
  definition = function(object, ...)
  {
    identity1 <- switch(class(object@data),
      RLBigDataDedup = object@data@identity,
      RLBigDataLinkage = object@data@identity1)

    identity2 <- switch(class(object@data),
      RLBigDataDedup = object@data@identity,
      RLBigDataLinkage = object@data@identity2)
    # TP: true positive, FP: false positive, TN: true negative,
    # FN: false negative, PM: possible links that are matches,
    # PN: possible links that are non-matches
    TP <- sum(identity1[object@links[,1]]==identity2[object@links[,2]], na.rm=TRUE)
    FP <- sum(identity1[object@links[,1]]!=identity2[object@links[,2]], na.rm=TRUE)
    PM <- sum(identity1[object@possibleLinks[,1]]==identity2[object@possibleLinks[,2]], na.rm=TRUE)
    PN <- sum(identity1[object@possibleLinks[,1]]!=identity2[object@possibleLinks[,2]], na.rm=TRUE)

    nMatch <- getMatchCount(object@data)
    nUnknown <- getNACount(object@data)
    FN <- nMatch - TP - PM
    TN <- object@nPairs - TP - FN - FP - PM - PN - nUnknown
    return(list(
      alpha=FN/(TP+FN),
      beta=FP/(TN+FP),
      accuracy=(TP+TN)/(TP+TN+FP+FN),
      precision=TP/(TP+FP),
      sensitivity=TP/(TP+FN),
      specificity=TN/(TN+FP),
      ppv=TP/(TP+FP),
      npv=TN/(TN+FN)
    ))
  }
)

# wrapper for new S4 method for backward compatibility
errorMeasures <- function(result)
{
  if (!("RecLinkResult" %in% class(result)))
      stop(sprintf("Wrong type for result: %s!", class(result)))
  getErrorMeasures(result)
}
