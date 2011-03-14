#' Large deduplication data set
#'
#' Realization of RLBigData for deduplication of a single data set. Records are
#' stored as rows in \code{data}. Two records \code{data[i,]} and {data[j,]} are 
#' considered equal if and only if \code{identity[i]==identity[j]}
#'
#' @slot data Records to deduplicate
#' @slot identity Identity vector. 
setClass(
  Class = "RLResult",
  representation = representation(
    data = "RLBigData",
    links = "matrix",
    possibleLinks = "matrix",
    nPairs = "numeric"
  ),
  prototype = prototype(
    data = NULL,
    links = matrix(numeric(0), ncol=2, nrow=0),
    possibleLinks = matrix(numeric(0), ncol=2, nrow=0),
    nPairs = numeric(0)
  )
)

# no constructor, is created by classifying methods

#setMethod(
#  f = "show",
#  signature = "RLResult",
#  definition = function(object)
#  {
#        
#  }
#)
#

setGeneric(
  name = "getTable",
  def = function(object, ...) standardGeneric("getTable")
)

# constructs a contengency table of matches
setMethod(
  f = "getTable",
  signature = "RecLinkResult",
  definition = function(object, ...)
  {
    TP=length(which(object$pairs$is_match & object$prediction=="L")) # true positive
    FP=length(which(!object$pairs$is_match & object$prediction=="L")) # false positive
    TN=length(which(!object$pairs$is_match & object$prediction=="N")) # true negative
    FN=length(which(object$pairs$is_match & object$prediction=="N")) # false negative

    tab <- table(as.logical(object$pairs$is_match),object$prediction,
            dnn=list("true status","classification"),useNA="ifany")
    # if "NA" row appears in the table (for pairs with unknown true status),
    # put them in the middle
    if (nrow(tab) == 3)
      tab[c(1,3,2),]
    else
      tab
  }
)

setMethod(
  f = "getTable",
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
    matchPossible <- sum(identity1[object@possibleLinks[,1]]==identity2[object@possibleLinks[,2]], na.rm=TRUE)
    FN <- nMatch - TP - matchPossible
    NAPossible <- sum(is.na(identity1[object@possibleLinks[,1]]==identity2[object@possibleLinks[,2]]))
    nonmatchPossible <- nrow(object@possibleLinks) - NAPossible - matchPossible
    NALink <- sum(is.na(identity1[object@links[,1]]==identity2[object@links[,2]]))
    NANonLink <- getNACount(object@data) - NALink - NAPossible
    TN <- object@nPairs - sum(TP, FP, NALink, matchPossible, nonmatchPossible,
      NAPossible, FN, NANonLink)
    tab <- matrix(c(TN, NANonLink, FN, nonmatchPossible,
      NAPossible, matchPossible, FP, NALink, TP), ncol=3, nrow=3,
      dimnames = list('true status' = c("FALSE", NA, "TRUE"),
                          'classification' = c("N", "P", "L")))
    # remove row with NAs (unknown matching status) if there are no such pairs
    if (all(tab[2,]==0))
      tab <- tab[-2,]
    as.table(tab)
  }
)

