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

errorMeasures <- function(result)
{
  if (!("RecLinkResult" %in% class(result)))
      stop(sprintf("Wrong type for result: %s!", class(result)))

  TP=length(which(result$pairs$is_match & result$prediction=="L")) # true positive
  FP=length(which(!result$pairs$is_match & result$prediction=="L")) # false positive
  TN=length(which(!result$pairs$is_match & result$prediction=="N")) # true negative
  FN=length(which(result$pairs$is_match & result$prediction=="N")) # false negative
    
  return(list(
    alpha=FN/(TP+FN),
    beta=FP/(TN+FP),
    accuracy=(TP+TN)/(TP+TN+FP+FN),
    precision=TP/(TP+FP),
    sensitivity=TP/(TP+FN),
    specificity=TN/(TN+FP)
  ))
}
