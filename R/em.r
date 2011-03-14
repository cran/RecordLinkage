# moved to em-methods.r:
#
# emWeights <- function (rpairs, cutoff=0.95,...)
#
#emClassify <- function (rpairs,threshold.upper=Inf, 
#                        threshold.lower=threshold.upper,my=Inf, ny=Inf)



optimalThreshold <- function (rpairs, my=NaN, ny=NaN)
{
  if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
    stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

  if (nrow(rpairs$pairs) == 0)
    stop("No record pairs!")
  
  if (is.null(rpairs$Wdata))
    stop("No weights in rpairs!")

  if (!is.numeric(my))
    stop(sprintf("Illegal type for my: %s", class(my)))
  if (!missing(my) && (my < 0 || my > 1))
    stop(sprintf("Illegal value for my: %g", my))

  if (!is.numeric(ny))
    stop(sprintf("Illegal type for ny: %s", class(ny)))
  if (!missing(ny) && (ny < 0 || ny > 1))
    stop(sprintf("Illegal value for ny: %g", ny))

  # remove missing values for matching status
  indMissing <- which(is.na(rpairs$pairs$is_match))
  if(length(indMissing)==nrow(rpairs$pairs))
    stop("Only pairs with unknown status in rpairs!")

  if (length(indMissing >0)) rpairs <- rpairs[-indMissing]
  
  
	o=order(rpairs$Wdata,decreasing=TRUE)
	weights=rpairs$Wdata[o]
  n_data=length(weights)
	is_match=rpairs$pairs$is_match[o]
	n_match <- sum(is_match==1, na.rm = TRUE)
	n_non_match <- sum(is_match==0, na.rm = TRUE)
	FP_err=cumsum(is_match!=1)#/as.numeric(1:n_data)
  FN_err=rev(cumsum(rev(is_match==1)))#/as.numeric(1:n_data)))
  error=FP_err+FN_err

  # nun baue Tabelle, in der Gewicht (unique) und Fehlerrate gegenübergestellt
  # sind. Die Fehlerrate eines Gewichts ist in der sortierten Tabelle gleich
  # der Fehlerrate für den letzten Datensatz des Blocks
  # tapply() sortiert aufsteigend, das rev() stellt 
  # die absteigende Reihenfolge wieder her

  error_unique=rev(tapply(error,weights,tail,1))
  FP_err_unique=rev(tapply(FP_err/n_non_match,weights,tail,1))
  FN_err_unique=rev(tapply(FN_err/n_match,weights,tail,1))
  
  weights_unique=unique(weights)

  # Bestimme Gewicht des Datensatzes mit minimalem Fehler.

  if (missing(my) && missing(ny))
    return(as.numeric(weights_unique[which.min(error_unique)]))

  if (!missing(my))
  {
    min_ind <- which.min(FN_err_unique[FP_err_unique<=my])
    return(as.numeric(weights_unique[FP_err_unique<=my][min_ind]))
  }

  if (!missing(ny))
  {
    min_ind <- which.min(FP_err_unique[FN_err_unique<=ny])
    return(as.numeric(weights_unique[FN_err_unique<=ny][min_ind]))
  }
}



