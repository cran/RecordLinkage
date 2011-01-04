# Tests for functions in file genSamples.r


.setUp <- function()
{
  data(RLdata500)
  rpairs <<- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
}

test.getMinimalTrain.exceptions <- function()
{

#  data(RLdata500)
#  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))

  # errors concering argument rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(getMinimalTrain(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(getMinimalTrain(rpairs2), msg = "wrong type for rpairs")
  
  
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[0,]
  checkException(getMinimalTrain(rpairs2), msg = "no data pairs")

  # errors concerning argument nEx
  checkException(getMinimalTrain(rpairs, nEx = -1),
    msg = "Illegal value for nEx")
    
  checkException(getMinimalTrain(rpairs, nEx = 0),
    msg = "Illegal value for nEx")

  checkException(getMinimalTrain(rpairs, nEx = 3+5i),
    msg = "Illegal value for nEx")
}

test.getMinimalTrain <- function()
{
#  data(RLdata500)
#  rpairs <- compare.dedup(RLdata500,identity = identity.RLdata500)
  minTrain <- getMinimalTrain(rpairs)
  # iterate all pairs, excluding id and match status
  pairs <- rpairs$pairs[,-c(1,2,ncol(rpairs$pairs))]
  pairs[is.na(pairs)] <- 0
  pairs <- unique(pairs)
  for (n_ex in c(1,3))
  {
    minPairs <- minTrain$pairs[,-c(1,2,ncol(minTrain$pairs))]
    for (p in 1:nrow(pairs))
    {
      # check number of examples for every pattern
      # can be less, but must be at least, maximum n_ex
      n_ex_result <- sum(apply(minPairs, 1, function(x) all(x==p)))
      checkTrue(n_ex_result >= 1 || n_ex_result <= n_ex,
        msg=sprintf(" Check minimal training set. Desired: %d examples, found %d.",
          n_ex, n_ex_result))
      
    }
  }
  
  # test the bug that pairs with unknown status become non-matches
  rpairs2 <- compare.dedup(RLdata500)
  minTrain <- getMinimalTrain(rpairs2)
  checkTrue(all(is.na(minTrain$pairs$is_match)),
    msg = " Test for bug: conversion of unknown to non-match")
}

test.splitData.exceptions <- function()
{
#  data(RLdata500)
#  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))

  # errors concerning dataset

  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(splitData(rpairs2, 0.1), msg = "wrong class for dataset")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(splitData(rpairs2, 0.1), msg = "wrong type for dataset")
  
  
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[0,]
  checkException(splitData(rpairs2, 0.1), msg = "no data in dataset")


  # errors concerning prop
  
  checkException(splitData(rpairs, -0.2), msg = "Illegal value for prop")
  checkException(splitData(rpairs, 0), msg = "Illegal value for prop")
  checkException(splitData(rpairs, 1), msg = "Illegal value for prop")
  checkException(splitData(rpairs, 1.2), msg = "Illegal value for prop")
  checkException(splitData(rpairs, 0.1+2i), msg = "Illegal value for prop")

  # errors concerning mprop
  
  # errors concerning num.non
  checkException(splitData(rpairs, num.non=-1, des.mprop=0.1),
    msg = "Illegal value for num.non")  
  checkException(splitData(rpairs, num.non=30+10i, des.mprop=0.1),
    msg = "Illegal value for num.non")  
  checkException(splitData(rpairs, num.non=TRUE, des.mprop=0.1),
    msg = "Illegal type for num.non")  
  checkException(splitData(rpairs, num.non=FALSE, des.mprop=0.1),
    msg = "Illegal value for num.non")  

  # errors concerning des.mprop
  checkException(splitData(rpairs, num.non=100, des.mprop=-0.1),
    msg = "Illegal value for des.mprop")  
  checkException(splitData(rpairs, num.non=100, des.mprop=0.4i),
    msg = "Illegal value for des.mprop")  

  # errors concerning use.pred
  
  # errors concerning dataset and prop
  
  # errors concerning match information with use.pred=FALSE    
  rpairs2 <- compare.dedup(RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  checkException(splitData(rpairs2, prop=0.2, keep.mprop=TRUE),
    msg = "Using keep.mprop without match information")
  checkException(splitData(rpairs2, num.non=1000, des.mprop=0.02),
    msg = "Using des.mprop without match information")

  # errors concerning prediction with use.pred=TRUE    
  checkException(splitData(rpairs, prop=0.2, keep.mprop=TRUE, use.pred=TRUE),
    msg = "Using keep.mprop and use.pred=TRUE without prediction")
  checkException(splitData(rpairs, num.non=1000, des.mprop=0.02, use.pred=TRUE),
    msg = "Using des.mprop and use.pred=TRUE without prediction")

  # errors concerning dataset and num.non
  checkException(splitData(rpairs, num.non=1000, des.mprop=0.01),
    msg = "Not enough non-matches")
}


test.splitData <- function()
{

  # tests for use.pred = FALSE
    # test usage with prop
      # keep.mprop = FALSE
      prop <- 0.3
      result <- splitData(rpairs, prop=prop, keep.mprop=FALSE)
      ndata <- nrow(rpairs$pairs)
      ndata1 <- nrow(result$train$pairs)
      ndata2 <- nrow(result$valid$pairs)
      checkEqualsNumeric(ndata1, round(ndata * prop))
      checkEqualsNumeric(ndata2, round(ndata * (1-prop)))
      # check if record pairs and matching status are correctly assigned
      order_all <- do.call(order,rpairs$pairs[,1:2])
      patterns_all <- rpairs$pairs[order_all,]
      order_result <- do.call(order,rbind(result$train$pairs[,1:2],
                                  result$valid$pairs[,1:2]))
      patterns_result <- rbind(result$train$pairs,
                               result$valid$pairs)[order_result,]
      checkEquals(patterns_all, patterns_result)
      # keep.mprop = TRUE
      result <- splitData(rpairs, prop=prop, keep.mprop=TRUE)
      ndata <- nrow(rpairs$pairs)
      nMatches <- sum(rpairs$pairs$is_match,na.rm=TRUE)
      ndata1 <- nrow(result$train$pairs)
      nMatches1 <- sum(result$train$pairs$is_match,na.rm=TRUE)
      ndata2 <- nrow(result$valid$pairs)
      nMatches2 <- sum(result$valid$pairs$is_match,na.rm=TRUE)
      checkEqualsNumeric(ndata1, round(ndata * prop))
      checkEqualsNumeric(nMatches1, round(nMatches * prop))
      checkEqualsNumeric(ndata2, round(ndata * (1-prop)))
      checkEqualsNumeric(nMatches2, round(nMatches * (1-prop)))

    # test usage with num.non/des.mprop
      num.non <- 100
      des.mprop <- 0.1
      result <- splitData(rpairs, num.non=num.non, des.mprop=des.mprop)
      nNonMatchTrain <- sum(!result$train$pairs$is_match, na.rm=TRUE)
      nMatchTrain <- sum(result$train$pairs$is_match, na.rm=TRUE)
      checkEquals(nNonMatchTrain, num.non,
        msg = "Check number of matches with num.non and des.mprop")
      checkEquals(nMatchTrain, nNonMatchTrain * des.mprop,
        msg = "Check number of non-matches with num.non and des.mprop")
      # check that no record pair is included in both result sets
      trainIds <- apply(result$train$pairs[,1:2], 1, paste, collapse=" ")
      validIds <- apply(result$valid$pairs[,1:2], 1, paste, collapse=" ")
      checkEquals(intersect(trainIds, validIds), character(0),
        msg = "Check that train and valid are disjoint.")
      # check that all record pairs appear somewhere in the result and
      # patterns are assigned correctly
      order_all <- do.call(order,rpairs$pairs[,1:2])
      patterns_all <- rpairs$pairs[order_all,]
      order_result <- do.call(order,rbind(result$train$pairs[,1:2],
                                  result$valid$pairs[,1:2]))
      patterns_result <- rbind(result$train$pairs,
                               result$valid$pairs)[order_result,]
      checkEquals(patterns_all, patterns_result,
        msg = "check that records are assigned correctly")
      

  # tests for use.pred = TRUE
  rpairs <- epiWeights(rpairs)
  rpairs <- epiClassify(rpairs, optimalThreshold(rpairs))
    # test usage with prop, keep.mprop = TRUE
    result <- splitData(rpairs, prop=prop, keep.mprop=TRUE, use.pred=TRUE)
    # check if factor (prediction) was split correctly (was a bug)
    checkEquals(class(result$train$prediction), "factor")
    checkEquals(levels(result$train$prediction), c("N", "P", "L"))
    checkEquals(class(result$valid$prediction), "factor")
    checkEquals(levels(result$valid$prediction), c("N", "P", "L"))

    # check if predictions are assigned correctly
    order_all <- do.call(order,rpairs$pairs[,1:2])
    prediction_all <- rpairs$prediction[order_all]
    order_result <- do.call(order,rbind(result$train$pairs[,1:2],
                                result$valid$pairs[,1:2]))
    prediction_result <- c(result$train$prediction,
                             result$valid$prediction)[order_result]                                 
    # cast to numeric because factor levels are lost after c()
    checkEquals(as.numeric(prediction_all),as.numeric(prediction_result))

    # check that number of matches etc. is correct
    ndata <- nrow(rpairs$pairs)
    nMatches <- sum(rpairs$prediction=="L")
    ndata1 <- nrow(result$train$pairs)
    nMatches1 <- sum(result$train$prediction=="L")
    ndata2 <- nrow(result$valid$pairs)
    nMatches2 <- sum(result$valid$prediction=="L")
    checkEqualsNumeric(ndata1, round(ndata * prop))
    checkEqualsNumeric(nMatches1, round(nMatches * prop))
    checkEqualsNumeric(ndata2, round(ndata * (1-prop)))
    checkEqualsNumeric(nMatches2, round(nMatches * (1-prop)))

  # test usage with num.non, des.mprop    
    num.non <- 100
    des.mprop <- 0.1
    result <- splitData(rpairs, num.non=num.non, des.mprop=des.mprop, 
      use.pred = TRUE)
    nNonMatchTrain <- sum(result$train$prediction == "N")
    nMatchTrain <- sum(result$train$prediction == "L")
    checkEquals(nNonMatchTrain, num.non,
      msg = "Check number of matches with num.non and des.mprop")
    checkEquals(nMatchTrain, nNonMatchTrain * des.mprop,
      msg = "Check number of non-matches with num.non and des.mprop")
    # check that no record pair is included in both result sets
    trainIds <- apply(result$train$pairs[,1:2], 1, paste, collapse=" ")
    validIds <- apply(result$valid$pairs[,1:2], 1, paste, collapse=" ")
    checkEquals(intersect(trainIds, validIds), character(0),
      msg = "Check that train and valid are disjoint.")
    # check that all record pairs appear somewhere in the result and
    # patterns are assigned correctly
    order_all <- do.call(order,rpairs$pairs[,1:2])
    patterns_all <- rpairs$pairs[order_all,]
    order_result <- do.call(order,rbind(result$train$pairs[,1:2],
                                result$valid$pairs[,1:2]))
    patterns_result <- rbind(result$train$pairs,
                             result$valid$pairs)[order_result,]
    checkEquals(patterns_all, patterns_result,
      msg = "check that records are assigned correctly")
      
      
  # checks for special cases

    # not enough matches: use maximum number
    result <- splitData(rpairs, num.non=200, des.mprop=0.5)
    nMatches <- sum(rpairs$pairs$is_match)
    nMatchesResult <- sum(result$train$pairs$is_match, na.rm=TRUE)
    checkEqualsNumeric(nMatches, nMatchesResult)
}


test.genSamples.exceptions <- function()
{
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500)
  
  # errors concerning dataset

  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(genSamples(rpairs2, 0.1), msg = "wrong class for dataset")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(genSamples(rpairs2, 0.1), msg = "wrong type for dataset")
  
  
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[0,]
  checkException(genSamples(rpairs2, 0.1), msg = "no data in dataset")

    # errors concerning num.non
  checkException(genSamples(rpairs, num.non=-1, des.mprop=0.1),
    msg = "Illegal value for num.non")  
  checkException(genSamples(rpairs, num.non=30+10i, des.mprop=0.1),
    msg = "Illegal value for num.non")  
  checkException(genSamples(rpairs, num.non=TRUE, des.mprop=0.1),
    msg = "Illegal type for num.non")  
  checkException(genSamples(rpairs, num.non=FALSE, des.mprop=0.1),
    msg = "Illegal value for num.non")  

  # errors concerning des.mprop
  checkException(genSamples(rpairs, num.non=100, des.mprop=-0.1),
    msg = "Illegal value for des.mprop")  
  checkException(genSamples(rpairs, num.non=100, des.mprop=0.4i),
    msg = "Illegal value for des.mprop")  

}

test.genSamples <- function()
{
  DEACTIVATED(msg = "Only delegates calls, no testing.")
}
