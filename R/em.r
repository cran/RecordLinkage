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

  # grouped by weight, count number of non-matches and matches
  errStatFun <- function(x)
  {
      nMatch <- sum(x)
      nNonMatch <- length(x) - sum(x)
      list(nMatch = nMatch, nNonMatch = nNonMatch)
  }
  dt <- data.table(is_match=rpairs$pairs$is_match,
    Wdata=match(rpairs$Wdata, sort(rpairs$W)), key="Wdata")
  errStat <- dt[,errStatFun(is_match), by=Wdata]

  # Count false negatives and positives per weight. The vectors correspond to
  # unique(sort(rpairsWdata)). Each position holds
  #   for FN: the number of false negatives if all record pairs with
  #           smaller or equal weight are classified as non-links
  #   for FP: the number of false positives if all record pairs with
  #           greater or equal weight are classified as links
  FN <- cumsum(errStat$nMatch)
  FP <- rev(cumsum(rev(errStat$nNonMatch)))

  # Construct error measures therefrom. Because thresholds define
  # right-closed and left-open intervals, the range of weights has to be extended
  # by a value greater than all exisiting weights (the case that no pairs are
  # classified as links at all). The error rates are extended by zeros.
  alphaErr <- c(0, FN/sum(errStat$nMatch))
  betaErr <- c(FP/sum(errStat$nNonMatch), 0)
  accuracy <- (nrow(rpairs$pairs) - (FP + FN)) / nrow(rpairs$pairs)

  classWeights <- c(sort(unique(rpairs$Wdata)), Inf)

  # set thresholds

  # no error bounds given: maximize accuracy
  if (missing(my) && missing(ny))
    return(as.numeric(classWeights[which.max(accuracy)]))

  # only bound for alpha error given: minimize beta error under constraint that
  # bound for alpha error is met
  if (!missing(ny))
  {
    min_ind <- which.min(betaErr[alphaErr<=ny])
    return(as.numeric(classWeights[alphaErr<=ny][min_ind]))
  }

  # only bound for beta error given: minimize alpha error under constraint that
  # bound for beta error is met
  if (!missing(my))
  {
    min_ind <- which.min(alphaErr[betaErr<=my])
    return(as.numeric(classWeights[betaErr<=my][min_ind]))
  }
}
