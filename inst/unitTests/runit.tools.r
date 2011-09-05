# Test File for Package Record Linkage
#
# Test Functions in tools.r

test.unorderedPairs <- function()
{
  # illegal cases
  checkException(unorderedPairs("a")) # single value, not a natural number
  checkException(unorderedPairs(TRUE)) # single value, not a natural number
  checkException(unorderedPairs(NA)) # single value, not a natural number
  checkException(unorderedPairs(0.5)) # single value, not a natural number
  checkException(unorderedPairs(2+1i)) # single value, not a natural number
  checkException(unorderedPairs(-3)) # single value, not a natural number
  checkException(unorderedPairs(1)) # single value, not a natural number > 1

  checkException(unorderedPairs(factor("a", "b", "c"))) # factors not allowed

  # trivial case: single pair for 2 elements
  checkEquals(unorderedPairs(2), matrix(1:2, 2, 1))
  checkEquals(unorderedPairs(c("a", "b")), matrix(c("a", "b"), 2, 1))
  
  # n=4: six pairs
  checkEquals(unorderedPairs(4), matrix(c(1,2,1,3,1,4,2,3,2,4,3,4),2,6))
  
  # pairs from individual character elements
  checkEquals(unorderedPairs(c("a", "b", "c")), 
    matrix(c("a", "b", "a", "c", "b", "c"), 2, 3))

  # also handles NA 
  checkEquals(unorderedPairs(c(TRUE, FALSE, NA)), 
    matrix(c(TRUE, FALSE, TRUE, NA, FALSE, NA), 2, 3))
    
}


test.isFALSE <- function()
{
  x <- FALSE
  checkEquals(isFALSE(x), TRUE)
  x <- TRUE
  checkEquals(isFALSE(x), FALSE)
  x <- 0
  checkEquals(isFALSE(x), FALSE)
  x <- "FALSE"
  checkEquals(isFALSE(x), FALSE)
  x <- c("FALSE", "FALSE")
  checkEquals(isFALSE(x), FALSE)
}

test.delete.NULLs <- function()
{
  x <- list(1,c(2,3), NULL, list("A","B"))
  checkEquals(delete.NULLs(x), list(1,c(2,3), list("A","B")))
  x <- list(1,c(2,3), "NULL", list("A","B"))
  checkEquals(delete.NULLs(x), x)
  x <- list(1,c(2,3), NA, list("A","B"))
  checkEquals(delete.NULLs(x), x)
  x <- list(1,c(2,3), NaN, list("A","B"))
  checkEquals(delete.NULLs(x), x)  
}

test.resample <- function()
{
  checkEquals(resample(2,1),2)
  checkEquals(resample("A",0), character(0))
  checkEquals(resample(numeric(0),0), numeric(0))
  set.seed(1)
  result1 <- sample(1:10,3)
  set.seed(1)
  result2 <- resample(1:10,3)
  checkEquals(result1, result2)
}

test.subset <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, blockfld = list(1,3, 5:7),
    identity = identity.RLdata500)
  rpairs <- epiWeights(rpairs)
  nPairs <- nrow(rpairs$pairs)

  s <- sample(nPairs, nPairs / 2)
  rpairsSamp <- rpairs[s]

  checkEquals(rpairs$data, rpairsSamp$data)
  checkEquals(rpairs$frequencies, rpairsSamp$frequencies)
  checkEquals(rpairs$type, rpairsSamp$type)
  checkEquals(rpairs$pairs[s,], rpairsSamp$pairs)
  checkEquals(rpairs$Wdata[s], rpairsSamp$Wdata)
  
  result <- epiClassify(rpairs, optimalThreshold(rpairs))
  resultSamp <- result[s]

  checkEquals(result$data, resultSamp$data)
  checkEquals(result$frequencies, resultSamp$frequencies)
  checkEquals(result$type, resultSamp$type)
  checkEquals(result$pairs[s,], resultSamp$pairs)
  checkEquals(result$Wdata[s], resultSamp$Wdata)
  checkEquals(result$prediction[s], resultSamp$prediction)

}

test.getExpectedSize.data.frame <- function()
{
  # set up test data of the following form:
  #  x1,x2
  #  1,1
  #  1,1
  #  2,1
  #  2,2
  #  3,1
  #  3,2
  testdat <- as.data.frame(matrix(c(1,1,2,2,3,3,1,1,1,2,1,2), ncol=2))
  names(testdat) <- c("x1", "x2")
  # Blocking on x1 gives 3 pairs
  checkEquals(getExpectedSize(testdat, 1), 3)

  # Blocking on x2 gives 7 pairs
  checkEquals(getExpectedSize(testdat, 2), 7)

  # Blocking with x1 and x2 (union): expected size should be
  # 15 [possible number of combinations]
  # * (1 -  [probability of complementary event]
  #    (4/5 [chance of not passing the first blocking]
  #    * 8/15) [chance of not passing the second blocking]
  #   ) = 8.6, rounded 9
  checkEquals(getExpectedSize(testdat, list(1,2)), 9)

  # Blocking with x1 and x2 (intersect): only 1 pair
  checkEquals(getExpectedSize(testdat, c(1,2)), 1)
}


test.getExpectedSize.RLBigDataDedup <- function()
{
  # set up test data of the following form:
  #  x1,x2
  #  1,1
  #  1,1
  #  2,1
  #  2,2
  #  3,1
  #  3,2
  testdat <- as.data.frame(matrix(c(1,1,2,2,3,3,1,1,1,2,1,2), ncol=2))
  names(testdat) <- c("x1", "x2")
  # Blocking on x1 gives 3 pairs
  checkEquals(getExpectedSize(RLBigDataDedup(testdat, blockfld=1)), 3)

  # Blocking on x2 gives 7 pairs
  checkEquals(getExpectedSize(RLBigDataDedup(testdat, blockfld=2)), 7)

  # Blocking with x1 and x2 (union): expected size should be
  # 15 [possible number of combinations]
  # * (1 -  [probability of complementary event]
  #    (4/5 [chance of not passing the first blocking]
  #    * 8/15) [chance of not passing the second blocking]
  #   ) = 8.6, rounded: 9
  checkEquals(getExpectedSize(RLBigDataDedup(testdat, blockfld=list(1,2))), 9)

  # Blocking with x1 and x2 (intersect): only 1 pair
  checkEquals(getExpectedSize(RLBigDataDedup(testdat, blockfld=c(1,2))), 1)
  
  # check for bug that caused function to fail if one of the blocking columns
  # does not have any matching values
  testdat2 <- cbind(testdat, 1:nrow(testdat))
  names(testdat2) <- c("x1", "x2", "x3")
  checkEquals(getExpectedSize(RLBigDataDedup(testdat2, blockfld=list(1,3))), 3)

  # for NULL values
  testdat2$x3 <- NA
  checkEquals(getExpectedSize(RLBigDataDedup(testdat2, blockfld=list(1,3))), 3)

}


test.getExpectedSize.RLBigDataLinkage <- function()
{
  # set up test data of the following form:
  #  x1,x2
  #  1,1
  #  2,1
  #  2,2
  #  3,1
  #  3,2
  testdat <- as.data.frame(matrix(c(1,2,2,3,3,1,1,2,1,2), ncol=2))
  names(testdat) <- c("x1", "x2")
  # Blocking on x1 gives 9 pairs
  checkEquals(getExpectedSize(RLBigDataLinkage(testdat, testdat, blockfld=1)), 9)

  # Blocking on x2 gives 13 pairs
  checkEquals(getExpectedSize(RLBigDataLinkage(testdat, testdat, blockfld=2)), 13)

  # Blocking with x1 and x2 (union): expected size should be
  # 25 [possible number of combinations]
  # * (1 -  [probability of complementary event]
  #    (16/25 [chance of not passing the first blocking]
  #    * 12/25) [chance of not passing the second blocking]
  #   ) = 17.32, rounded 17
  checkEquals(getExpectedSize(RLBigDataLinkage(testdat, testdat, blockfld=list(1,2))), 17)

  # Blocking with x1 and x2 (intersect): 5 pairs
  checkEquals(getExpectedSize(RLBigDataLinkage(testdat, testdat, blockfld=c(1,2))), 5)

  # check for bug that caused function to fail if one of the blocking columns
  # does not have any matching values (NULL values)
  testdat2$x3 <- NA
  checkEquals(getExpectedSize(RLBigDataLinkage(testdat2, testdat2, blockfld=list(1,3))), 9)

}


test.append <- function()
{
  data(RLdata500)
  s <- sample(500, 250)
  rpairs1 <- compare.dedup(RLdata500[s,], identity = identity.RLdata500[s])
  rpairs2 <- compare.dedup(RLdata500[-s,], identity = identity.RLdata500[-s])
  rpairs1 <- epiWeights(rpairs1)
  rpairs2 <- epiWeights(rpairs2)
  testResult <- rpairs1 %append% rpairs2
  
  
  checkEquals(testResult$pairs, rbind(rpairs1$pairs, rpairs2$pairs))
  checkEquals(testResult$Wdata, c(rpairs1$Wdata, rpairs2$Wdata))
  
  result1 <- epiClassify(rpairs1, 0.7, 0.5)
  result2 <- epiClassify(rpairs1, 0.7, 0.5)
  testResult <- result1 %append% result2
  
  checkEquals(testResult$pairs, rbind(result1$pairs, result2$pairs))
  checkEquals(testResult$Wdata, c(result1$Wdata, result2$Wdata))
  checkEquals(as.character(testResult$prediction),
    c(as.character(result1$prediction), as.character(result2$prediction)))
  checkEquals(class(testResult$prediction), "factor")
  checkEquals(levels(testResult$prediction), c("N", "P", "L"))
}

test.summary <- function()
{
  # The output of summary is matched against the most important information
  # on the data set

  data1 <- read.table("data1.compare.txt", sep=",", header=TRUE)
  identity1 <- scan("identity1.summary.txt", sep=",")
  rpairs <- compare.dedup(data1, identity = identity1)
  testResult <- capture.output(summary(rpairs))
  # the data at hand should contain 1 match, 4 unknown pairs and 5 non-matches
  checkTrue(any(grepl("Deduplication", testResult)))
  checkTrue(any(grepl("5 records", testResult)))
  checkTrue(any(grepl("10 record pairs", testResult)))
  checkTrue(any(grepl("1 match", testResult)))
  checkTrue(any(grepl("5 non-matches", testResult)))
  checkTrue(any(grepl("4 pairs with unknown status", testResult)))

  # create result object
  result <- rpairs
  result$prediction <- c("L", "L", "L", "P", "P", "N", "N", "N", "N", "N")
  class(result) <- c("RecLinkResult", "RecLinkData")
  testResult <- capture.output(summary(result))
  checkTrue(any(grepl("3 links", testResult)))
  checkTrue(any(grepl("5 non-links", testResult)))
  checkTrue(any(grepl("2 possible links", testResult)))

  # check linkage data
  data2 <- read.table("data2.compare.txt", sep=",", header=TRUE)
  data3 <- read.table("data3.compare.txt", sep=",", header=TRUE)
  identity2 <- scan("identity2.compare.txt", sep=",", comment.char="#")
  identity3 <- scan("identity3.compare.txt", sep=",", comment.char="#")
  rpairs <- compare.linkage(data2, data3, identity1=identity2, identity2=identity3)
  testResult <- capture.output(summary(rpairs))
  checkTrue(any(grepl("Linkage", testResult)))
  checkTrue(any(grepl("3 records", testResult))) # first data set
  checkTrue(any(grepl("2 records", testResult))) # second data set
  checkTrue(any(grepl("6 record pairs", testResult))) # second data set
  checkTrue(any(grepl("1 match", testResult)))
  checkTrue(any(grepl("5 non-matches", testResult)))
  checkTrue(any(grepl("0 pairs with unknown status", testResult)))

}

test.show <- function()
{
  data1 <- read.table("data1.compare.txt", sep=",", header=TRUE)
  identity1 <- scan("identity1.summary.txt", sep=",")
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  testResult <- capture.output(show(rpairs))
  checkTrue(any(grepl("Deduplication", testResult)))
  checkTrue(any(grepl("5 records", testResult)))
# deactivated: show was modified, this information is now in summary()
#  checkTrue(any(grepl("1 match", testResult)))
#  checkTrue(any(grepl("4 pairs with unknown status", testResult)))

  # linkage data
  data2 <- read.table("data2.compare.txt", sep=",", header=TRUE)
  data3 <- read.table("data3.compare.txt", sep=",", header=TRUE)
  identity2 <- scan("identity2.compare.txt", sep=",", comment.char="#")
  identity3 <- scan("identity3.compare.txt", sep=",", comment.char="#")
  rpairs <- RLBigDataLinkage(data2, data3, identity1=identity2, identity2=identity3)
  testResult <- capture.output(show(rpairs))
  checkTrue(any(grepl("Linkage", testResult)))
  checkTrue(any(grepl("3 records", testResult))) # first data set
  checkTrue(any(grepl("2 records", testResult))) # second data set
# deactivated: show was modified, this information is now in summary()
#  checkTrue(any(grepl("1 match", testResult)))
#  checkTrue(any(grepl("0 pairs with unknown status", testResult)))
}