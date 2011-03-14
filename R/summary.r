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
    cat("\n")
    cat(sprintf("%d links detected",length(which(object$prediction=="L"))),"\n")
    cat(sprintf("%d possible links detected",length(which(object$prediction=="P"))),"\n")
    cat(sprintf("%d non-links detected",length(which(object$prediction=="N"))),"\n")
 
    cat("\n")

    TP=length(which(object$pairs$is_match & object$prediction=="L")) # true positive
    FP=length(which(!object$pairs$is_match & object$prediction=="L")) # false positive
    TN=length(which(!object$pairs$is_match & object$prediction=="N")) # true negative
    FN=length(which(object$pairs$is_match & object$prediction=="N")) # false negative
    
    alpha=FN/(TP+FN)
    beta=FP/(TN+FP)
    accuracy=(TP+TN)/(TP+TN+FP+FN)
    cat(sprintf("alpha error: %f\n",alpha))
    cat(sprintf("beta error: %f\n",beta))
    cat(sprintf("accuracy: %f\n",accuracy))
    cat("\n\n")
    cat("Classification table:\n\n")
    print(table(as.logical(object$pairs$is_match),object$prediction,
          dnn=list("true status","classification"),useNA="ifany"))
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


    # TODO: blocking information

    # if weights are stored in DB, the exact number of pairs is readily available
    if (dbExistsTable(con, "Wdata"))
    {
      npairs <- dbGetQuery(con, "select count(*) as c from Wdata")$c
      cat(sprintf("%d record pairs\n",npairs))
    } else
    {
#      npairs <- getExpectedSize(object@data, object@blockFld)
 #     cat(sprintf("Approximately %d record pairs\n", round(npairs)))
    }

    cat("\n")

  	cat(sprintf("%d matches\n",
          getMatchCount(object)))
    	cat(sprintf("%d pairs with unknown status\n",
            getNACount(object)))
    cat("\n")

  	if (dbExistsTable(con, "Wdata"))
  	{
    # TODO: check performance for very large sets
      Wdata <- dbGetQuery(con, "select W from Wdata")$W
  		cat("Weight distribution:\n\n")
  		h=hist(Wdata,plot=FALSE)
  		c=h$counts
  		# nehme Gewichtsintervalle als Indizes, um Histogrammansicht zu erhalten
      names(c)=sapply(1:(length(h$breaks)-1),
        function(x) sprintf("(%g,%g]",h$breaks[x],h$breaks[x+1]))
      # erstes Intervall ist auch links geschlossen
      names(c)[1]=sprintf("[%g,%g]", h$breaks[1], h$breaks[2])
  		print(c)
    }
   }
)


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
    # FN: false negative
    TP <- sum(identity1[object@links[,1]]==identity2[object@links[,2]], na.rm=TRUE)
    FP <- sum(identity1[object@links[,1]]!=identity2[object@links[,2]], na.rm=TRUE)
    nMatch <- getMatchCount(object@data)
    FN <- nMatch - TP
    TN <- object@nPairs - TP - FN - FP
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
