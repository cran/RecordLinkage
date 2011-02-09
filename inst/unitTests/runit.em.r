# Tests for functions in file em.r

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
  result1 <- emWeights(rpairs1)
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
  result2 <- emWeights(rpairs2, cutoff = 1)
  checkEqualsNumeric(result1$W, result2$W)
  checkEqualsNumeric(result1$Wdata, result2$Wdata)
  
  # test individual cutoff values for each attribute
  rpairs3 <- rpairs2
  # copy of fuzzy pairs with binary first attribute
  rpairs3$pairs$fname_c1 <- (rpairs3$pairs$fname_c1==1) * 1
  # different cutoff value for binary attribute should make no difference
  result3 <- emWeights(rpairs3,cutoff=0.7)
  result4 <- emWeights(rpairs3,cutoff=c(0.5,rep(0.7,6)))
  checkEqualsNumeric(result3$Wdata,result4$Wdata,
    msg = "check usage of individual cutoff values")
  # different cutoff value for fuzzy column should yield different weights
  result5 <- emWeights(rpairs3,cutoff=c(0.7, 0.7, 0.5, rep(0.7,4)))
  checkTrue(!identical(result3$Wdata, result5$Wdata),
      msg = "check usage of individual cutoff values")
}

test.emClassify.exceptions <- function()
{
  load("rpairs.em.rda")
  
  # illegal class, type of rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(emClassify(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(emClassify(rpairs2), msg = "wrong type for rpairs")

  # no weights in rpairs
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  checkException(emClassify(rpairs2), msg = "no weights in rpairs")

  
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

test.emClassify <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs <- emWeights(rpairs)

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
  
  my=runif(1,0.01, 0.2)
  thresh <- optimalThreshold(rpairs, my=my)
  alpha_result <- errorMeasures(emClassify(rpairs, thresh))$alpha
  W_unique <- unique(rpairs$Wdata)
  alpha=list()
  for (thresh_iter in W_unique)
  {
    errM <- errorMeasures(emClassify(rpairs, thresh_iter))
    if (errM$beta <= my)
    {
      alpha[[as.character(thresh_iter)]] <- errM$alpha
    }
  } 
  checkEqualsNumeric(alpha_result, min(unlist(alpha),na.rm=TRUE))  


  # corresponding check for ny. threshold.upper must be used to obtain
  # legal beta errors
  ny=runif(1,0.01, 0.2)
  thresh <- optimalThreshold(rpairs, ny=ny)
  beta_result <- errorMeasures(emClassify(rpairs, thresh))$beta
  W_unique <- unique(rpairs$Wdata)
  beta_all=list()
  for (thresh_iter in W_unique)
  {
    errM <- errorMeasures(emClassify(rpairs, thresh_iter))
    if (errM$alpha <= ny)
    {
      beta_all[[as.character(thresh_iter)]] <- errM$beta
    }
  } 
  checkEqualsNumeric(beta_result, min(unlist(beta_all), na.rm=TRUE))  
  
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
