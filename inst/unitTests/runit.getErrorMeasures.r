# Test method for S3 objects of class "RecLinkResult"
test.getErrorMeasures.RecLinkResult <- function()
{
  # use data from compare test
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- scan("identity1.compare.txt",comment.char="#",sep=",")
  # blocking yields 5 record pairs
  rpairs <-  compare.dedup(data1)
  class(rpairs) <- "RecLinkResult"


  # illegal calls
  rpairs2 <- rpairs
  class(rpairs2) <- "RecLinkData"
  checkException(errorMeasures(rpairs2), msg = "Wrong class for rpairs")


  # assign matching status suitable for test (actual data do not matter)
  rpairs$pairs$is_match <- c(rep(1,6),rep(0,4))
  rpairs$prediction <- factor(c(rep("L",4),"N","N",rep("L",3),"N"), levels=c("N","P","L"))
  # result is a table of the form
  #
  #   1 3
  #   2 4
  #
  # which yields distinct values for the error measures
  measures <- errorMeasures(rpairs)
  checkEqualsNumeric(measures$alpha,1/3) # 2 FN / 6 matches
  checkEqualsNumeric(measures$beta,3/4)  # 3 FP / 4 non-matches
  checkEqualsNumeric(measures$accuracy,5/10) # 5 correctly class. / 10 total
  checkEqualsNumeric(measures$precision,4/7) # 4 TP / 7 classified as links
  checkEqualsNumeric(measures$sensitivity,2/3) # 4 TP / 6 matches
  checkEqualsNumeric(measures$specificity,1/4) # 1 TN / 4 non-matches

  # include possible link, this will not be counted
  rpairs$pairs <- rbind(rpairs$pairs, rpairs$pairs[1:2,])
  rpairs$prediction <-factor(c(rep("L",4),"N","N",rep("L",3),"N","P","P"),
    levels=c("N","P","L"))

  # check that wrapper function and S4 method return the same result
  checkEquals(errorMeasures(rpairs), getErrorMeasures(rpairs))
}



runit.getErrorMeasures.RLResult <- function()
{
  # create a test object
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  # we need more matches in this case
  # this means we have pairs (1,2), (3,4), (3,5) and (4,5) as matches
  identity1 <- c(1,1,3,3,3)
  rpairs <- RLBigDataDedup(data1, identity = identity1)


  # select pairs as links such there are distinct numbers for false positives etc.
  # leads to 3 TP, 2 FP, 1 FN, 4 TN
  linkInd <- matrix(c(1,2,3,4,3,5,1,3,1,4), ncol = 2, nrow = 5, byrow = TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10)

  # result is a table of the form
  #
  #   4 2
  #   1 3
  #
  # which yields distinct values for the error measures
  measures <- list(
    alpha = 1/4,      # 1 FN / 4 matches
    beta = 1/3,       # 2 FP / 6 non-matches
    accuracy = 7/10,  # 7 correct / 10 total
    precision = 3/5,  # 3 TP / 5 classified as link
    sensitivity = 3/4, # 3 TP / 4 matches
    specificity = 2/3 # 4 TN / 6 non-matches
  )

  checkEquals(measures, getErrorMeasures(result))

  # check that possible links are not counted
  # test by removing one true link
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  linkInd <- matrix(c(3,4,3,5,1,3,1,4), ncol = 2, nrow = 5, byrow = TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10,
    possibleLinks = matrix(c(1,2),ncol=2, nrow=1))

  # result is a table of the form
  #
  #   4 0 2
  #   1 1 2
  #
  # which yields the follwoing error measures:
  measures <- list(
    alpha = 1/4,      # 1 FN / 3 matches
    beta = 1/3,       # 2 FP / 6 non-matches
    accuracy = 2/3,  # 6 correct / 9 total
    precision = 1/2,  # 2 TP / 4 classified as link
    sensitivity = 1/3, # 2 TP / 3 matches
    specificity = 2/3 # 4 TN / 6 non-matches
  )

  # check that pairs with unknown status are not counted
  identity1 <- c(NA,1,3,3,3)
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  linkInd <- matrix(c(1,2,3,4,3,5,1,3,1,4), ncol = 2, nrow = 5, byrow = TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10,
    possibleLinks = matrix(c(1,2),ncol=2, nrow=1))

  # result is a table of the form
  #
  #   4 0 2
  #   1 1 2
  #
  # which yields the follwoing error measures:
  measures <- list(
    alpha = 1/4,      # 1 FN / 3 matches
    beta = 1/3,       # 2 FP / 6 non-matches
    accuracy = 2/3,  # 6 correct / 9 total
    precision = 1/2,  # 2 TP / 4 classified as link
    sensitivity = 1/3, # 2 TP / 3 matches
    specificity = 2/3 # 4 TN / 6 non-matches
  )

}