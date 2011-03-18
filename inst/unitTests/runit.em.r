# Tests for functions in file em.r


# TODO: 
#   Test für EMWeights-Methode
#   Test für Berücksichtigung von exclude-Feldern (Warnung bei EMWeights-Methode?)

test.emWeights.exceptions <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))

  # errors concering argument rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(emWeights(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(emWeights(rpairs2), msg = "wrong type for rpairs")
  
  
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[0,]
  checkException(emWeights(rpairs2), msg = "no data pairs")
  
  # errors concerning argument cutoff
  checkException(emWeights(rpairs, cutoff = 1.2),
    msg = "cutoff too high")
  checkException(emWeights(rpairs, cutoff = -0.3),
    msg = "cutoff too low")  

  # RLBigData object with expired SQLite connection
  rpairsBig <- RLBigDataDedup(RLdata500)
  dbDisconnect(rpairsBig@con)
  checkException(emWeights(rpairsBig, tol=0.01), msg = "invalid SQLite connection")
}

test.emWeights <- function()
{
  data(RLdata500)
  rpairs1 <- compare.dedup(RLdata500, identity=identity.RLdata500, 
    blockfld=list(1,3,c(5,6,7)))
  # same set with string comparison
  rpairs2 <- compare.dedup(RLdata500, identity=identity.RLdata500, 
    blockfld=list(1,3,c(5,6,7)), strcmp = TRUE)
  # basic consistency checks
  result1 <- emWeights(rpairs1, tol=0.01)
  checkEquals(class(result1)[1], "RecLinkData",
    msg = " (check class of result)")
  checkTrue(is.numeric(result1$M),
    msg = "check M component")
  checkTrue(is.numeric(result1$U),
    msg = "check U component")
  checkTrue(is.numeric(result1$W),
    msg = "check W component")
  checkTrue(is.numeric(result1$Wdata),
    msg = "check Wdata component")
  checkTrue(all(result1$Wdata %in% result1$W),
    msg = "check that Wdata is subset of W")
  checkTrue(!any(is.na(result1$Wdata)),
    msg = "check that no weights are NaN or NA")
  # use fuzzy set with cutoff 1, should yield same result
  result2 <- emWeights(rpairs2, cutoff = 1, tol=0.01)
  checkEqualsNumeric(result1$W, result2$W)
  checkEqualsNumeric(result1$Wdata, result2$Wdata)
  
  # test individual cutoff values for each attribute
  rpairs3 <- rpairs2
  # copy of fuzzy pairs with binary first attribute
  rpairs3$pairs$fname_c1 <- (rpairs3$pairs$fname_c1==1) * 1
  # different cutoff value for binary attribute should make no difference
  result3 <- emWeights(rpairs3,cutoff=0.7, tol=0.01)
  result4 <- emWeights(rpairs3,cutoff=c(0.5,rep(0.7,6)), tol=0.01)
  checkEqualsNumeric(result3$Wdata,result4$Wdata,
    msg = "check usage of individual cutoff values")
  # different cutoff value for fuzzy column should yield different weights
  result5 <- emWeights(rpairs3,cutoff=c(0.7, 0.7, 0.5, rep(0.7,4)), tol=0.01)
  checkTrue(!identical(result3$Wdata, result5$Wdata),
      msg = "check usage of individual cutoff values")
}


test.emWeights.RLBigData <- function()
{
  # checks that results are the same as for an equivalent RecLinkData-object
  data(RLdata500)
  rpairs1 <- compare.dedup(RLdata500, blockfld=list(1,3,5,6,7))
  rpairs1 <- emWeights(rpairs1, tol=0.01)
  rpairs2 <- RLBigDataDedup(RLdata500, blockfld=list(1,3,5,6,7))
  rpairs2 <- emWeights(rpairs2, tol=0.01)
  ids <- rpairs1$pairs[,1:2]
  Wdata2 <- dbGetPreparedQuery(rpairs2@con,
    "select W from Wdata where id1=? and id2=?", ids)$W
  checkEqualsNumeric(Wdata2, rpairs1$Wdata)

}

test.emClassify.exceptions <- function()
{

  # create various types of test data
  # also one object without weights stored
  data(RLdata500)
  rpairsBig1 <- RLBigDataDedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairsBig2 <- clone(rpairsBig1)
  rpairsBig1 <- emWeights(rpairsBig1, tol=0.01)
  rpairsBig2 <- emWeights(rpairsBig2, tol=0.01)
  load("rpairs.em.rda")

  # illegal class, type of rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(emClassify(rpairs2, my=0.05), msg = "wrong class for rpairs")
  checkException(emClassify(rpairs2, threshold.upper=10, threshold.lower=0), msg = "wrong class for rpairs")

  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(emClassify(rpairs2, my=0.05), msg = "wrong type for rpairs")
  checkException(emClassify(rpairs2, threshold.upper=10, threshold.lower=0), msg = "wrong type for rpairs")

  # no weights in rpairs
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  checkException(emClassify(rpairs2, my=0.05), msg = "no weights in rpairs")
  checkException(emClassify(rpairs2, threshold.upper=10, threshold.lower=0), msg = "no weights in rpairs")

  rpairs2 <- clone(rpairsBig1)
  dbGetQuery(rpairs2@con, "drop table W")
  checkException(emClassify(rpairs2, my=0.05), msg = "no weights in rpairs")
  checkException(emClassify(rpairs2, threshold.upper=10, threshold.lower=0), msg = "no weights in rpairs")

  rpairs2 <- clone(rpairsBig2)
  dbGetQuery(rpairs2@con, "drop table W")
  checkException(emClassify(rpairs2, my=0.05), msg = "no weights in rpairs")
  checkException(emClassify(rpairs2, threshold.upper=10, threshold.lower=0), msg = "no weights in rpairs")

  # run the following tests twice: also for RLBigData-object
  for (rpairs in list(rpairs, rpairsBig1, rpairsBig2))
  {

    # errors concerning threshold.upper
    checkException(emClassify(rpairs, threshold.upper = "0"),
      msg = "wrong type for threshold.upper")
    checkException(emClassify(rpairs, threshold.upper = FALSE),
      msg = "wrong type for threshold.upper")
    checkException(emClassify(rpairs, threshold.upper = 1+9i),
      msg = "wrong type for threshold.upper")

    # errors concerning threshold.lower
    checkException(emClassify(rpairs, threshold.lower = "0"),
      msg = "wrong type for threshold.lower")
    checkException(emClassify(rpairs, threshold.lower = FALSE),
      msg = "wrong type for threshold.lower")
    checkException(emClassify(rpairs, threshold.lower = 1+9i),
      msg = "wrong type for threshold.lower")

    # errors concerning combination of thresholds
    # runif will not generate 0 (see doc), is greater than 0
    checkException(emClassify(rpairs, threshold.upper=0, threshold.lower=runif(1)),
      msg = "lower threshold greater than upper threshold")

    # errors concerning my
    checkException (emClassify(rpairs, my=-2), msg = "Illegal value for my")
    checkException (emClassify(rpairs, my=1+runif(1)), msg = "Illegal value for my")
    checkException (emClassify(rpairs, my="0.2"), msg = "Illegal value for my")
    checkException (emClassify(rpairs, my=TRUE), msg = "Illegal value for my")

    # errors concerning ny
    checkException (emClassify(rpairs, ny=-2), msg = "Illegal value for ny")
    checkException (emClassify(rpairs, ny=1+runif(1)), msg = "Illegal value for ny")
    checkException (emClassify(rpairs, ny="0.2"), msg = "Illegal value for ny")
    checkException (emClassify(rpairs, ny=TRUE), msg = "Illegal value for ny")
  }
  # RLBigData object with expired SQLite connection
  rpairsBig <- RLBigDataDedup(RLdata500, blockfld = list(1,3,5,6,7))
  rpairsBig <- emWeights(rpairsBig, tol=0.01)
  dbDisconnect(rpairsBig@con)
  checkException(emWeights(rpairsBig, tol=0.01), msg = "invalid SQLite connection")

}

test.emClassify <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs <- emWeights(rpairs, tol=0.01)

  # test threshold
  # only upper threshold supplied (only matches and non-matches)
  # feasible value
  thresh <- sample(unique(rpairs$Wdata),1)
  result <- emClassify(rpairs, threshold.upper = thresh)
  checkTrue(all((result$Wdata >= thresh)==(result$prediction=="L")),
    msg = "check links, only upper threshold, feasible value")
  checkTrue(all((result$Wdata < thresh)==(result$prediction=="N")),
    msg = "check non-links, only upper threshold, feasible value")
  # high value: only non-matches
  thresh <- max(rpairs$Wdata + 1)
  result <- emClassify(rpairs, threshold.upper = thresh)
  checkTrue(all(result$prediction=="N"),
    msg = "check high value for only upper threshold")
  # low value: only matches
  thresh <- min(rpairs$Wdata)
  result <- emClassify(rpairs, threshold.upper = thresh)
  checkTrue(all(result$prediction=="L"),
    msg = "check low value for only lower threshold")

  # only lower threshold supplied (only non-matches and possibles
  # feasible value
  thresh <- sample(unique(rpairs$Wdata),1)
  result <- emClassify(rpairs, threshold.lower = thresh)
  checkTrue(all((result$Wdata >= thresh)==(result$prediction=="P")),
    msg = "check possibles, only lower threshold, feasible value")    
  checkTrue(all((result$Wdata < thresh)==(result$prediction=="N")),
    msg = "check non-links, only lower threshold, feasible value")        
  # high value: only non-matches
  thresh <- max(rpairs$Wdata + 1)
  result <- emClassify(rpairs, threshold.lower = thresh)
  checkTrue(all(result$prediction=="N"),
    msg = "check high value for only lower threshold")
  # low value: only possible matches
  thresh <- min(rpairs$Wdata)
  result <- emClassify(rpairs, threshold.lower = thresh)
  checkTrue(all(result$prediction=="P"),
    msg = "check low value for only lower threshold")

  # upper threshold with lower threshold = -Inf (only possibles and matches)
  # feasible value
  thresh <- sample(unique(rpairs$Wdata),1)
  result <- emClassify(rpairs, threshold.upper = thresh,
    threshold.lower = -Inf)
  checkTrue(all((result$Wdata >= thresh)==(result$prediction=="L")),
    msg = "check links, upper threshold and lower = -Inf, feasible value")        
  checkTrue(all((result$Wdata < thresh)==(result$prediction=="P")),
    msg = "check links, upper threshold and lower = -Inf, feasible value")            
  # high value: only possibles
  thresh <- max(rpairs$Wdata + 1)
  result <- emClassify(rpairs, threshold.upper = thresh,
    threshold.lower = -Inf)
  checkTrue(all(result$prediction=="P"),
    msg = "check high value for upper threshold and lower = -Inf")
  # low value: only matches
  thresh <- min(rpairs$Wdata)
  result <- emClassify(rpairs, threshold.upper = thresh,
    threshold.lower = -Inf)
  checkTrue(all(result$prediction=="L"),
    msg = "check high value for upper threshold and lower = -Inf")

  # Test usage of my / ny error bounds. For this purpose, an object is populated
  # with real values for m- and u-probabilites. In this case the error bounds
  # should be valid.
  pairs <- as.matrix(rpairs$pairs[-c(1,2)])
  pairs [is.na(pairs)] <- 0
  counts <- countpattern(pairs)
  countNonMatch <- counts[seq(1, length(counts) - 1, 2)]
  countMatch <- counts[seq(2, length(counts), 2)]
  realM <- countMatch / sum(countMatch)
  realU <- countNonMatch / sum(countNonMatch)
  rpairs$M <- realM
  rpairs$U <- realU

  # First check: bound on beta error (alias my)
  W_unique <- unique(rpairs$Wdata)
  allBeta <- unique(sapply(W_unique, function(thresh_iter)
    {
      errorMeasures(emClassify(rpairs, thresh_iter))$beta
    }
  ))

  for (my in allBeta)
  {
    alpha_result <- errorMeasures(emClassify(rpairs, my=my))$alpha
    alpha=list()
    for (thresh_iter in W_unique)
    {
      errM <- errorMeasures(emClassify(rpairs, thresh_iter))
      if (errM$beta <= my)
      {
        alpha[[as.character(thresh_iter)]] <- errM$alpha
      }
    }
    checkEqualsNumeric(alpha_result, min(unlist(alpha),na.rm=TRUE),
      msg = sprintf("check for my=%g", my))
  }

  # Second check: bound on alpha error (alias my)
  allAlpha <- unique(sapply(W_unique, function(thresh_iter)
    {
      errorMeasures(emClassify(rpairs, thresh_iter))$alpha
    }
  ))

  for (ny in allAlpha)
  {
    beta_result <- errorMeasures(emClassify(rpairs, ny=ny))$beta
    beta=list()
    for (thresh_iter in W_unique)
    {
      errM <- errorMeasures(emClassify(rpairs, thresh_iter))
      if (errM$alpha <= ny)
      {
        beta[[as.character(thresh_iter)]] <- errM$beta
      }
    }
    checkEqualsNumeric(beta_result, min(unlist(beta),na.rm=TRUE),
      msg = sprintf("check for ny=%g", ny))
  }

  # Third check: both bounds set to reasonable values
  # take values for error bounds which are closest to the 10% quantile
  ny <- allAlpha[which.min(abs(allAlpha - quantile(allAlpha, 0.1)))]
  my <- allBeta[which.min(abs(allBeta - quantile(allBeta, 0.1)))]
  # calculate error measures from the contingency table,
  # the value in getErrorMeasures values possible links differently
  resultTab <- getTable(emClassify(rpairs, my=my, ny=ny))
  checkEqualsNumeric(my, resultTab["FALSE", "L"] / sum(resultTab["FALSE",]))
  checkEqualsNumeric(ny, resultTab["TRUE", "N"] / sum(resultTab["TRUE",]))
}

test.emClassify.RLBigData <- function()
{
  # create two test objects, on with weights stored per pattern, one
  # for which weights will be calculated on the fly
  data(RLdata500)
  rpairs1 <- RLBigDataDedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs2 <-clone(rpairs1)
  rpairs1 <- emWeights(rpairs1, tol=0.01)
  rpairs2 <- emWeights(rpairs2, tol=0.01, store.weights = FALSE)
  # same record pairs as RecLinkData object, needed for some checks below
  rpairs3 <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs3 <- emWeights(rpairs3, tol=0.01)
  Wdata <- dbReadTable(rpairs1@con, "Wdata")

  for (rpairs in list(rpairs1, rpairs2))
  {
  # test threshold
  # only upper threshold supplied (only matches and non-matches)
    # feasible value
    thresh <- sample(unique(Wdata$W),1)
    result <- emClassify(rpairs, threshold.upper = thresh)
    reqLinks <- Wdata[Wdata$W >= thresh, 1:2]
    checkEqualsNumeric(result@links[order(result@links[,1], result@links[,2]),],
      as.matrix(reqLinks[order(reqLinks$id1, reqLinks$id2), ]),
      msg = "check links, only upper threshold, feasible value")

    checkEquals(nrow(result@possibleLinks), 0,
      msg = "check possible links, only upper threshold, feasible value")

    # high value: only non-matches
    thresh <- max(Wdata$W + 1)
    result <- emClassify(rpairs, threshold.upper = thresh)
   checkEquals(nrow(result@links), 0,
      msg = "check high value for only upper threshold")
   checkEquals(nrow(result@possibleLinks), 0,
      msg = "check high value for only upper threshold")

    # low value: only matches
    thresh <- min(Wdata$W)
    result <- emClassify(rpairs, threshold.upper = thresh)
    checkEqualsNumeric(as.matrix(Wdata[order(Wdata$id1, Wdata$id2), 1:2]),
      result@links[order(result@links[,1], result@links[,2]),],
      msg = "check low value for only higher threshold")

  # only lower threshold supplied (only non-matches and possibles
    # feasible value
    thresh <- sample(unique(Wdata$W),1)
    result <- emClassify(rpairs, threshold.lower = thresh)
    reqPossibleLinks <- Wdata[Wdata$W >= thresh, 1:2]

    checkEqualsNumeric(result@possibleLinks[order(result@possibleLinks[,1],
      result@possibleLinks[,2]),],
      as.matrix(reqPossibleLinks[order(reqPossibleLinks$id1, reqPossibleLinks$id2), ]),
      msg = "check possibles, only lower threshold, feasible value")

    checkEquals(nrow(result@links), 0,
      msg = "check links, only lower threshold, feasible value")


    # high value: only non-matches
    thresh <- max(Wdata$W + 1)
    result <- emClassify(rpairs, threshold.lower = thresh)
    checkEquals(nrow(result@links), 0,
      msg = "check high value for only lower threshold")
    checkEquals(nrow(result@possibleLinks), 0,
      msg = "check high value for only lower threshold")

    # low value: only possible matches
    thresh <- min(Wdata$W)
    result <- emClassify(rpairs, threshold.lower = thresh)
    checkEqualsNumeric(as.matrix(Wdata[order(Wdata$id1, Wdata$id2), 1:2]),
      result@possibleLinks[order(result@possibleLinks[,1], result@possibleLinks[,2]),],
      msg = "check low value for only lower threshold")

  # upper threshold with lower threshold = -Inf (only possibles and matches)
    # feasible value
    thresh <- sample(unique(Wdata$W),1)
    result <- emClassify(rpairs, threshold.upper = thresh,
      threshold.lower = -Inf)
    reqLinks <- Wdata[Wdata$W >= thresh, 1:2]
    reqPossibleLinks <- Wdata[Wdata$W < thresh, 1:2]
    checkEqualsNumeric(result@links[order(result@links[,1], result@links[,2]),],
      as.matrix(reqLinks[order(reqLinks$id1, reqLinks$id2), ]),
      msg = "check links, upper threshold and lower = -Inf, feasible value")
    checkEqualsNumeric(result@possibleLinks[order(result@possibleLinks[,1],
      result@possibleLinks[,2]),],
      as.matrix(reqPossibleLinks[order(reqPossibleLinks$id1, reqPossibleLinks$id2), ]),
      msg = "check links, upper threshold and lower = -Inf, feasible value")

    # high value: only possibles
    thresh <- max(Wdata$W + 1)
    result <- emClassify(rpairs, threshold.upper = thresh,
      threshold.lower = -Inf)
    checkEqualsNumeric(as.matrix(Wdata[order(Wdata$id1, Wdata$id2), 1:2]),
      result@possibleLinks[order(result@possibleLinks[,1], result@possibleLinks[,2]),],
      msg = "check high value for upper threshold and lower = -Inf")


    # low value: only matches
    thresh <- min(Wdata$W)
    result <- emClassify(rpairs, threshold.upper = thresh,
      threshold.lower = -Inf)
    checkEqualsNumeric(as.matrix(Wdata[order(Wdata$id1, Wdata$id2), 1:2]),
      result@links[order(result@links[,1], result@links[,2]),],
      msg = "check low value for upper threshold and lower = -Inf")

  } # end for loop

  # Test usage of my / ny error bounds.
  # Only check equal result for RecLinkData and RecLinkData method.
  # This suffices as a check for correct values is made for the RecLinkData
  # method

  # first check: only my
  my <- 0.05
  resultS3 <- emClassify(rpairs3, my=my)
  linkIds <- resultS3$pairs[resultS3$prediction=="L", 1:2]
  linkIds <- as.matrix(linkIds[order(linkIds[[1]], linkIds[[2]]),])
  for (rpairs in list(rpairs1, rpairs2))
  {
    resultS4 <- emClassify(rpairs, my=my)
    linkIdsS4 <- as.matrix(resultS4@links[order(resultS4@links[,1], resultS4@links[,2]),])
    checkEqualsNumeric(linkIdsS4, linkIds, msg="check for my")
    checkEqualsNumeric(nrow(resultS4@possibleLinks), 0, msg="check for my")
  }



  # second check: only ny
  ny <- 0.05
  resultS3 <- emClassify(rpairs3, ny=ny)
  linkIds <- resultS3$pairs[resultS3$prediction=="L", 1:2]
  linkIds <- as.matrix(linkIds[order(linkIds[[1]], linkIds[[2]]),])
  for (rpairs in list(rpairs1, rpairs2))
  {
    resultS4 <- emClassify(rpairs, ny=ny)
    linkIdsS4 <- as.matrix(resultS4@links[order(resultS4@links[,1], resultS4@links[,2]),])
    checkEqualsNumeric(linkIdsS4, linkIds, msg="check for ny")
    checkEqualsNumeric(nrow(resultS4@possibleLinks), 0, msg="check for ny")
  }

  # third check: my and ny
  # second check: only my
  my <- 0.01
  ny <- 0.01
  resultS3 <- emClassify(rpairs3, my=my, ny=ny)
  linkIds <- resultS3$pairs[resultS3$prediction=="L", 1:2]
  linkIds <- as.matrix(linkIds[order(linkIds[[1]], linkIds[[2]]),])
  possibleLinkIds <- resultS3$pairs[resultS3$prediction=="P", 1:2]
  possibleLinkIds <- as.matrix(possibleLinkIds[order(possibleLinkIds[[1]],
    possibleLinkIds[[2]]),])
  for (rpairs in list(rpairs1, rpairs2))
  {
    resultS4 <- emClassify(rpairs, my=my, ny=ny)
    linkIdsS4 <- as.matrix(resultS4@links[order(resultS4@links[,1], resultS4@links[,2]),])
    possibleLinkIdsS4 <- as.matrix(resultS4@possibleLinks[order(resultS4@possibleLinks[,1],
      resultS4@possibleLinks[,2]),])
    checkEqualsNumeric(linkIdsS4, linkIds, msg="check for ny")
    checkEqualsNumeric(possibleLinkIdsS4, possibleLinkIds, msg="check for ny")
  }

}

test.optimalThreshold.exceptions <- function()
{
  load("rpairs.em.rda")
  
  # illegal class, type of rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(optimalThreshold(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(optimalThreshold(rpairs2), msg = "wrong type for rpairs")

  # no weights in rpairs
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  checkException(optimalThreshold(rpairs2), msg = "no weights in rpairs")

  # no pairs with known matching status
  rpairs2 <- rpairs
  is.na(rpairs2$pairs$is_match) <- TRUE
  checkException(optimalThreshold(rpairs2), msg = "only unknown pairs in rpairs")

  # errors concerning my
  checkException (optimalThreshold(rpairs, my=-2), msg = "Illegal value for my")
  checkException (optimalThreshold(rpairs, my=1+runif(1)), msg = "Illegal value for my")
  checkException (optimalThreshold(rpairs, my="0.2"), msg = "Illegal value for my")
  checkException (optimalThreshold(rpairs, my=TRUE), msg = "Illegal value for my")

  # errors concerning ny
  checkException (optimalThreshold(rpairs, ny=-2), msg = "Illegal value for ny")
  checkException (optimalThreshold(rpairs, ny=1+runif(1)), msg = "Illegal value for ny")
  checkException (optimalThreshold(rpairs, ny="0.2"), msg = "Illegal value for ny")
  checkException (optimalThreshold(rpairs, ny=TRUE), msg = "Illegal value for ny")

}

test.optimalThreshold <- function()
{
  load("rpairs.em.rda")

  # test default case: minimize overall error
  # check that the value with maximal accuracy is selected
  thresh <- optimalThreshold(rpairs)
  acc_result <- errorMeasures(emClassify(rpairs, thresh))$accuracy
  W_unique <- unique(rpairs$Wdata)
  acc=numeric(length(W_unique))
  names(acc) <- W_unique
  for (thresh_iter in W_unique)
  {
    
    acc[as.character(thresh_iter)] <- errorMeasures(emClassify(rpairs, thresh_iter))$accuracy
  }
  checkEquals(max(acc), acc_result, msg = "check that maximal accuracy is found")

  # test with threshold for my/ny: check that the minimal alpha error 
  # for which the error bound holds is obtained 
  
  # calculate thresholds for all relevant values of beta
  W_unique <- unique(rpairs$Wdata)
  allBeta <- unique(sapply(W_unique, function(thresh_iter)
    {
      errorMeasures(emClassify(rpairs, thresh_iter))$beta
    }
  ))

  for (my in allBeta)
  {
    thresh <- optimalThreshold(rpairs, my=my)
    alpha_result <- errorMeasures(emClassify(rpairs, thresh))$alpha
    alpha=list()
    for (thresh_iter in W_unique)
    {
      errM <- errorMeasures(emClassify(rpairs, thresh_iter))
      if (errM$beta <= my)
      {
        alpha[[as.character(thresh_iter)]] <- errM$alpha
      }
    }
    checkEqualsNumeric(alpha_result, min(unlist(alpha),na.rm=TRUE),
      msg = sprintf("check for my=%g", my))
  }

  # corresponding check for ny. threshold.upper must be used to obtain
  # legal beta errors

  # calculate thresholds for all relevant values of alpha
  W_unique <- unique(rpairs$Wdata)
  allAlpha <- unique(sapply(W_unique, function(thresh_iter)
    {
      errorMeasures(emClassify(rpairs, thresh_iter))$alpha
    }
  ))

  for (ny in allAlpha)
  {
    thresh <- optimalThreshold(rpairs, ny=ny)
    beta_result <- errorMeasures(emClassify(rpairs, thresh))$beta
    beta_all=list()
    for (thresh_iter in W_unique)
    {
      errM <- errorMeasures(emClassify(rpairs, thresh_iter))
      if (errM$alpha <= ny)
      {
        beta_all[[as.character(thresh_iter)]] <- errM$beta
      }
    }
    checkEqualsNumeric(beta_result, min(unlist(beta_all), na.rm=TRUE),
      msg = sprintf("check for ny=%g", ny))
  }
  # pairs with NA should be ignored, i.e. the result should be the same as if
  # these pairs were missing
  rpairs2 <- rpairs
  nPairs <- nrow(rpairs$pairs)
  s <- sample(nPairs, nPairs / 2)
  rpairs3 <- rpairs[-s]
  is.na(rpairs2$pairs$is_match[s]) <- TRUE
  checkEquals(optimalThreshold(rpairs2), optimalThreshold(rpairs3),
    msg = "check that unknown pairs are ignored")

}
