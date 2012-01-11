.setUp <- function()
{
  data(RLdata500)
}


getWeights <<- function(object)
{
  dbReadTable(object@con, "Wdata")$W
}

test.fsWeights.exceptions <- function()
{
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))

  # errors concering argument rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(fsWeights(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(fsWeights(rpairs2), msg = "wrong type for rpairs")
  
  
  rpairs2 <- rpairs
  rpairs2$pairs <- rpairs$pairs[0,]
  checkException(fsWeights(rpairs2), msg = "no data pairs")
  
  # errors concerning argument m
  checkException(fsWeights(rpairs, m="0.9"),
    msg = "wrong type for m")
  checkException(fsWeights(rpairs, m=TRUE),
    msg = "wrong type for m")
  checkException(fsWeights(rpairs, m=0.1+0.3i),
    msg = "wrong type for m")

  checkException(fsWeights(rpairs, m=-0.2),
    msg = "illegal value for m")
  checkException(fsWeights(rpairs, e=1.2),
    msg = "illegal value for e")

  # check for single illegal values in vector
  nattr <- ncol(rpairs$pairs)-3
  checkException(fsWeights(rpairs, u=c(rep(0.01,nattr-1),1.4)),
    msg = "illegal value for u")
  checkException(fsWeights(rpairs, u=c(rep(0.01,nattr-1),-0.2)),
    msg = "illegal value for u")

  # errors concerning argument m
  checkException(fsWeights(rpairs, m="0.1"),
    msg = "wrong type for m")
  checkException(fsWeights(rpairs, m=TRUE),
    msg = "wrong type for m")
  checkException(fsWeights(rpairs, m=0.1+0.3i),
    msg = "wrong type for m")

  checkException(fsWeights(rpairs, m=-0.2),
    msg = "illegal value for m")
  checkException(fsWeights(rpairs, m=1.2),
    msg = "illegal value for m")

  # check for single illegal values in vector
  nattr <- ncol(rpairs$pairs)-3
  checkException(fsWeights(rpairs, m=c(rep(0.01,nattr-1),1.4)),
    msg = "illegal value for m")
  checkException(fsWeights(rpairs, m=c(rep(0.01,nattr-1),-0.2)),
    msg = "illegal value for m")


  # m < u for some attributes
  checkException(fsWeights(rpairs, m=c(0.99,0.5,0.9,0.5,0.98,0.92,0.97),
    u=0.55), msg = "m < u for some attributes")

}


test.fsWeights <- function()
{
  # set up test case
  rpairs <- list()
  rpairs$pairs <- read.table("data1.fsWeights.txt", sep=",",header=TRUE, na.strings="")
  rpairs$data <- data.frame()
  rpairs$type <- "linkage"
  class(rpairs) <- "RecLinkData"
  res <- fsWeights(rpairs, m=c(0.75,0.5,0.875,0.75), u=0.125)
  # compare with manually calculated weights (see data1.epilink.txt)
  w1 <- log((0.75 * 0.5 * 0.875 * 0.75) / (0.125 * 0.875 * 0.125 * 0.125), base=2)
  w2 <- log((0.75 * 0.5 * 0.125 * 0.25) / (0.125 * 0.875 * 0.875 * 0.875), base=2)
  checkEqualsNumeric(res$Wdata, c(w1, w2), msg="check weight calculation")

}


test.fsClassify.exceptions <- function()
{
  load("rpairs.em.rda")
  rpairs <- fsWeights(rpairs)

  # illegal class, type of rpairs
  rpairs2 <- rpairs
  class(rpairs2) <- "wrongClass"
  checkException(fsClassify(rpairs2), msg = "wrong class for rpairs")
  
  rpairs2 <- rpairs$pairs
  class(rpairs2) <- "RecLinkData"
  checkException(fsClassify(rpairs2), msg = "wrong type for rpairs")

  # no weights in rpairs
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  checkException(fsClassify(rpairs2), msg = "no weights in rpairs")

  
  # errors concerning threshold.upper
  checkException(fsClassify(rpairs, threshold.upper = "0"),
    msg = "wrong type for threshold.upper")
  checkException(fsClassify(rpairs, threshold.upper = FALSE),
    msg = "wrong type for threshold.upper")
  checkException(fsClassify(rpairs, threshold.upper = 1+9i),
    msg = "wrong type for threshold.upper")

  # errors concerning threshold.lower
  checkException(fsClassify(rpairs, threshold.lower = "0"),
    msg = "wrong type for threshold.lower")
  checkException(fsClassify(rpairs, threshold.lower = FALSE),
    msg = "wrong type for threshold.lower")
  checkException(fsClassify(rpairs, threshold.lower = 1+9i),
    msg = "wrong type for threshold.lower")

  # errors concerning combination of thresholds
  # runif will not generate 0 (see doc), is greater than 0
  checkException(fsClassify(rpairs, threshold.upper=0, threshold.lower=runif(1)),
    msg = "lower threshold greater than upper threshold")


}

test.fsClassify <- function()
{
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs <- fsWeights(rpairs)
  minWeight <- min(rpairs$Wdata)
  maxWeight <- max(rpairs$Wdata)
  
  # test with one threshold
  threshold.upper <- runif(1, minWeight, maxWeight)
  result <- fsClassify(rpairs, threshold.upper=threshold.upper)
  checkEquals(which(result$prediction=="L"), which(rpairs$Wdata >= threshold.upper),
    msg="check that all links have weight above threshold")
  checkEquals(which(result$prediction=="N"), which(rpairs$Wdata < threshold.upper),
    msg="check that all non-links have weight below threshold")
  
  # test with two thresholds
  threshold.upper <- runif(1, 0.6, maxWeight)
  threshold.lower <- runif(1, minWeight, 0.5)
  result <- fsClassify(rpairs, threshold.upper, threshold.lower)
  checkEquals(which(result$prediction=="L"), which(rpairs$Wdata >= threshold.upper),
    msg="check that all links have weight above threshold")
  checkEquals(which(result$prediction=="N"), which(rpairs$Wdata < threshold.lower),
    msg="check that all non-links have weight below threshold")
  checkEquals(which(result$prediction=="P"), 
    which(rpairs$Wdata < threshold.upper & rpairs$Wdata >=threshold.lower),
    msg="check weights of possible links")
    
  # check case with only links
  result <- fsClassify(rpairs, threshold.upper=minWeight)
  checkTrue(all(result$prediction=="L"))
  
  # check case with only non-links
  result <- fsClassify(rpairs, threshold.upper=maxWeight+0.1)
  checkTrue(all(result$prediction=="N"))
  
  # check case with only possible links
  result <- fsClassify(rpairs, maxWeight+0.1, minWeight)
  checkTrue(all(result$prediction=="P"))
  
}

test.fsClassify.RLBigData <- function()
{
  rpairs <- RLBigDataDedup(RLdata500, identity=identity.RLdata500, blockfld=list(5:6,6:7,c(5,7)))
  rpairs <- fsWeights(rpairs)
  Wdata <- as.ram(rpairs@Wdata)
  minWeight <- min(Wdata)
  maxWeight <- max(Wdata)

  # test with one threshold
  threshold.upper <- runif(1, minWeight, maxWeight)
  result <- fsClassify(rpairs, threshold.upper=threshold.upper)
  reqLinks <- which(Wdata >= threshold.upper)
  checkEqualsNumeric(which(as.ram(result@prediction) == "L"), reqLinks,
    msg = "check links, only upper threshold, feasible value")
  checkEqualsNumeric(sum(as.ram(result@prediction) == "P"), 0,
    msg = "check possible links, only upper threshold, feasible value")


  # test with two thresholds
  threshold.upper <- runif(1, 0.6, maxWeight)
  threshold.lower <- runif(1, minWeight, 0.5)
  result <- fsClassify(rpairs, threshold.upper, threshold.lower)
  reqLinks <- which(Wdata >= threshold.upper)
  reqPossibleLinks <- which(Wdata >= threshold.lower & Wdata < threshold.upper)

  checkEqualsNumeric(which(as.ram(result@prediction) == "L"), reqLinks,
    msg = "check links, two thresholda, feasible value")
  checkEqualsNumeric(which(as.ram(result@prediction) == "P"), reqPossibleLinks,
    msg = "check possible links, two thresholds, feasible value")


  # check case with only links
  result <- fsClassify(rpairs, threshold.upper=minWeight)
  checkTrue(all(as.ram(result@prediction) == "L"),
    msg = "check links, only upper threshold, only links")


  # check case with only non-links
  result <- fsClassify(rpairs, threshold.upper=maxWeight+0.1)
  checkTrue(all(as.ram(result@prediction) == "N"),
    msg = "check non-links, only upper threshold, only non-links")


  # check case with only possible links
  result <- fsClassify(rpairs, maxWeight+0.1, minWeight)
  checkTrue(all(as.ram(result@prediction) == "P"),
    msg = "check possible links, two thresholds, only possible links")
}

test.fsWeights.RLBigDataDedup <- function()
{
  rpairs <- RLBigDataDedup(RLdata500, blockfld=list(1,3,5,6,7), strcmp=1:4)
  rpairs <- fsWeights(rpairs)

  # generate control object with same weights
  rpairs2 <- fsWeights(compare.dedup(RLdata500, blockfld=list(1,3,5,6,7), strcmp=1:4))

  # Both methods should generate the same weights.
  checkEqualsNumeric(as.ram(rpairs@Wdata[fforder(rpairs@pairs$id1, rpairs@pairs$id2)]),
    rpairs2$Wdata[order(rpairs2$pairs$id1, rpairs2$pairs$id2)],
    msg = "check that weights are equal to those of S3 method")

}