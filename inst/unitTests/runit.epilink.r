test.epiWeights.exceptions <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))

  # errors concering argument rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(epiWeights(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(epiWeights(rpairs2), msg = "wrong type for rpairs")
  
  
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[0,]
  checkException(epiWeights(rpairs2), msg = "no data pairs")
  
  # errors concerning argument e
  checkException(epiWeights(rpairs, e="0.1"),
    msg = "wrong type for e")
  checkException(epiWeights(rpairs, e=TRUE),
    msg = "wrong type for e")
  checkException(epiWeights(rpairs, e=0.1+0.3i),
    msg = "wrong type for e")

  checkException(epiWeights(rpairs, e=-0.2),
    msg = "illegal value for e")
  checkException(epiWeights(rpairs, e=1),
    msg = "illegal value for e")
  checkException(epiWeights(rpairs, e=1.2),
    msg = "illegal value for e")
  # check for single illegal values in vector
  nattr <- ncol(rpairs$pairs)-3
  checkException(epiWeights(rpairs, e=c(rep(0.01,nattr-1),1.4)),
    msg = "illegal value for e")
  checkException(epiWeights(rpairs, e=c(rep(0.01,nattr-1),-0.2)),
    msg = "illegal value for e")

  # errors concerning argument f
  checkException(epiWeights(rpairs, f="0.1"),
    msg = "wrong type for f")
  checkException(epiWeights(rpairs, f=TRUE),
    msg = "wrong type for f")
  checkException(epiWeights(rpairs, f=0.1+0.3i),
    msg = "wrong type for f")

  checkException(epiWeights(rpairs, f=-0.2),
    msg = "illegal value for f")
  checkException(epiWeights(rpairs, f=0),
    msg = "illegal value for f")
  checkException(epiWeights(rpairs, f=1.2),
    msg = "illegal value for f")
  # check for single illegal values in vector
  nattr <- ncol(rpairs$pairs)-3
  checkException(epiWeights(rpairs, f=c(rep(0.01,nattr-1),1.4)),
    msg = "illegal value for f")
  checkException(epiWeights(rpairs, f=c(rep(0.01,nattr-1),-0.2)),
    msg = "illegal value for f")


  # illegal error rate ( e > 1-f)
  checkException(epiWeights(rpairs, f=c(0.01,0.5,0.1,0.5,0.02,0.08,0.03), 
    e=0.55), msg = "error rate does not satisfy e <= 1-f")
  

}


test.epiWeights <- function()
{
  # set up test case
  rpairs <- list()
  rpairs$pairs <- read.table("data1.epilink.txt", sep=",",header=TRUE, na.strings="")
  rpairs$data <- data.frame()
  rpairs$frequencies <- c(0.25,0.5,0.125,0.25)
  rpairs$type <- "linkage"
  class(rpairs) <- "RecLinkData"
  res <- epiWeights(rpairs, e=0.5)  
  # compare with manually calculated weights (see data1.epilink.txt)
  checkEqualsNumeric(res$Wdata, c(1,0.55), msg="check weight calculation")

  # same with f passed as arg
  
    
  # Test data with only NA/0 in one column
  # must be same result as when column is not present
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[,-4]
  rpairs2$frequencies <- rpairs$frequencies[-2]
  result1 <- epiWeights(rpairs2)
  rpairs3 <- rpairs
  rpairs3$pairs[,4]=c(0,0)
  result2 <- epiWeights(rpairs3)  

  # This causes a warning due to NaN weights
  warn <- getOption("warn")
  options(warn = 2)
  checkException(epiWeights(rpairs, e=0.5,f=0.5),
    msg = "warning for illegal weights")
  options(warn = warn)    

  # this caused an error because values were not recycled
  res <- epiWeights(rpairs, f=0.25)
  checkEqualsNumeric(res$Wdata, c(0.75, 0.45),
    msg = "check weight calculation with external f")

  # check range of weights for a suitable data set
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, strcmp=TRUE)
  rpairs <- epiWeights(rpairs)
  checkTrue(all(rpairs$Wdata >= 0 & rpairs$Wdata <=1),
    msg = "check weight range for large data set")
}


test.epiClassify.exceptions <- function()
{
  load("rpairs.em.rda")
  rpairs <- epiWeights(rpairs)

  # illegal class, type of rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(epiClassify(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(epiClassify(rpairs2), msg = "wrong type for rpairs")

  # no weights in rpairs
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  checkException(epiClassify(rpairs2), msg = "no weights in rpairs")

  
  # errors concerning threshold.upper
  checkException(epiClassify(rpairs, threshold.upper = "0"),
    msg = "wrong type for threshold.upper")
  checkException(epiClassify(rpairs, threshold.upper = FALSE),
    msg = "wrong type for threshold.upper")
  checkException(epiClassify(rpairs, threshold.upper = 1+9i),
    msg = "wrong type for threshold.upper")

  # errors concerning threshold.lower
  checkException(epiClassify(rpairs, threshold.lower = "0"),
    msg = "wrong type for threshold.lower")
  checkException(epiClassify(rpairs, threshold.lower = FALSE),
    msg = "wrong type for threshold.lower")
  checkException(epiClassify(rpairs, threshold.lower = 1+9i),
    msg = "wrong type for threshold.lower")

  # errors concerning combination of thresholds
  # runif will not generate 0 (see doc), is greater than 0
  checkException(epiClassify(rpairs, threshold.upper=0, threshold.lower=runif(1)),
    msg = "lower threshold greater than upper threshold")
  

}

test.epiClassify <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs <- epiWeights(rpairs)
  minWeight <- min(rpairs$Wdata)
  maxWeight <- max(rpairs$Wdata)
  
  # test with one threshold
  threshold.upper <- runif(1, minWeight, maxWeight)
  result <- epiClassify(rpairs, threshold.upper=threshold.upper)
  checkEquals(which(result$prediction=="L"), which(rpairs$Wdata >= threshold.upper),
    msg="check that all links have weight above threshold")
  checkEquals(which(result$prediction=="N"), which(rpairs$Wdata < threshold.upper),
    msg="check that all non-links have weight below threshold")
  
  # test with two thresholds
  threshold.upper <- runif(1, 0.6, maxWeight)
  threshold.lower <- runif(1, minWeight, 0.5)
  result <- epiClassify(rpairs, threshold.upper, threshold.lower)
  checkEquals(which(result$prediction=="L"), which(rpairs$Wdata >= threshold.upper),
    msg="check that all links have weight above threshold")
  checkEquals(which(result$prediction=="N"), which(rpairs$Wdata < threshold.lower),
    msg="check that all non-links have weight below threshold")
  checkEquals(which(result$prediction=="P"), 
    which(rpairs$Wdata < threshold.upper & rpairs$Wdata >=threshold.lower),
    msg="check weights of possible links")
    
  # check case with only links
  result <- epiClassify(rpairs, threshold.upper=minWeight)
  checkTrue(all(result$prediction=="L"))
  
  # check case with only non-links
  result <- epiClassify(rpairs, threshold.upper=maxWeight+0.1)
  checkTrue(all(result$prediction=="N"))
  
  # check case with only possible links
  result <- epiClassify(rpairs, maxWeight+0.1, minWeight)
  checkTrue(all(result$prediction=="P"))
  
}