test.getTable.RLResult <- function()
{
  # create a test object
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- c(1,1,3,3,3)
  rpairs <- RLBigDataDedup(data1, identity = identity1)


  # select pairs as links such there are distinct numbers for false positives etc.
  # leads to 3 TP, 2 FP, 1 FN, 4 TN
  linkInd <- matrix(c(1,2,3,4,3,5,1,3,1,4), ncol = 2, nrow = 5, byrow=TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10)

  # result should be a table of the form
  #
  #   4 0 2
  #   1 0 3
  #

  checkEqualsNumeric(getTable(result), matrix(c(4,1,0,0,2,3), nrow = 2, ncol = 3))


  # check with possible links
  # test by removing one true link
  identity1 <- c(1,1,3,3,3)
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  linkInd <- matrix(c(3,4,3,5,1,3,1,4), ncol = 2, nrow = 4, byrow=TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10,
    possibleLinks = matrix(c(1,2),ncol=2, nrow=1))

  # results in the follwing pairs:
  #
  # id1 id2 match? link?
  # 1   2   TRUE   P
  # 1   3   FALSE  L
  # 1   4   FALSE  L
  # 1   5   FALSE  N
  # 2   3   FALSE  N
  # 2   4   FALSE  N
  # 2   5   FALSE  N
  # 3   4   TRUE   L
  # 3   5   TRUE   L
  # 4   5   TRUE   N
  # result should be a table of the form
  #
  #   4 0 2
  #   1 1 2
  #

  checkEqualsNumeric(getTable(result), matrix(c(4,1,0,1,2,2), nrow = 2, ncol = 3))

  # check with pairs of unknown status
  identity1 <- c(NA,1,3,3,3)
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  linkInd <- matrix(c(1,2,3,4,3,5,1,3,1,4), ncol = 2, nrow = 5, byrow=TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10)

  # results in the follwing pairs:
  #
  # id1 id2 match? link?
  # 1   2   NA     L
  # 1   3   NA     L
  # 1   4   NA     L
  # 1   5   NA     N
  # 2   3   FALSE  N
  # 2   4   FALSE  N
  # 2   5   FALSE  N
  # 3   4   TRUE   L
  # 3   5   TRUE   L
  # 4   5   TRUE   N
  
  # result should be a table of the form
  #
  #   3 0 0
  #   1 0 3
  #   1 0 2
  #

  checkEqualsNumeric(getTable(result), matrix(c(3,1,1,0,0,0,0,3,2), nrow=3, ncol=3))

  # both possible links and missing identity information
  identity1 <- c(NA,1,3,3,3)
  rpairs <- RLBigDataDedup(data1, identity = identity1)
  linkInd <- matrix(c(3,4,3,5,1,3,1,4), ncol = 2, nrow = 4, byrow=TRUE)
  result <- new("RLResult", data = rpairs, links = linkInd, nPairs = 10,
    possibleLinks = matrix(c(1,2), nrow=1, ncol=2))

  # results in the follwing pairs:
  #
  # id1 id2 match? link?
  # 1   2   NA     P
  # 1   3   NA     L
  # 1   4   NA     L
  # 1   5   NA     N
  # 2   3   FALSE  N
  # 2   4   FALSE  N
  # 2   5   FALSE  N
  # 3   4   TRUE   L
  # 3   5   TRUE   L
  # 4   5   TRUE   N

  # result should be a table of the form
  #
  #   3 0 0
  #   1 1 2
  #   1 0 2
  #

  checkEqualsNumeric(getTable(result), matrix(c(3,1,1,0,1,0,0,2,2), nrow=3, ncol=3))

}


test.getTable.RecLinkResult <- function()
{
  # create a test object
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- c(1,1,3,3,3)
  rpairs <- compare.dedup(data1, identity = identity1)


  # select pairs as links such there are distinct numbers for false positives etc.
  # leads to 3 TP, 2 FP, 1 FN, 4 TN
  result <- rpairs
  class(result) <- "RecLinkResult"
  result$prediction <- factor(c("L", "L", "L", "N", # pairs (1,x)
         "N", "N", "N", # pairs (2,x)
         "L", "L", # pairs (3,x)
         "N"), levels=c("N", "P", "L")) # pair (4,5)

  # result should be a table of the form
  #
  #   4 0 2
  #   1 0 3
  #

  checkEqualsNumeric(getTable(result), matrix(c(4,1,0,0,2,3), nrow = 2, ncol = 3))


  # check with possible links
  # test by removing one true link
  # create a test object
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- c(1,1,3,3,3)
  rpairs <- compare.dedup(data1, identity = identity1)


  # select pairs as links such there are distinct numbers for false positives etc.
  # leads to 3 TP, 2 FP, 1 FN, 4 TN
  result <- rpairs
  class(result) <- "RecLinkResult"
  result$prediction <- factor(c("P", "L", "L", "N", # pairs (1,x)
         "N", "N", "N", # pairs (2,x)
         "L", "L", # pairs (3,x)
         "N"), levels=c("N", "P", "L")) # pair (4,5)



  # result should be a table of the form
  #
  #   4 0 2
  #   1 1 2
  #
  # which yields the follwoing error measures:

  checkEqualsNumeric(getTable(result), matrix(c(4,1,0,1,2,2), nrow = 2, ncol = 3))

  # check with pairs of unknown status

  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- c(NA,1,3,3,3)
  rpairs <- compare.dedup(data1, identity = identity1)


  # select pairs as links such there are distinct numbers for false positives etc.
  # leads to 3 TP, 2 FP, 1 FN, 4 TN
  result <- rpairs
  class(result) <- "RecLinkResult"
  result$prediction <- factor(c("L", "L", "L", "N", # pairs (1,x)
         "N", "N", "N", # pairs (2,x)
         "L", "L", # pairs (3,x)
         "N"), levels=c("N", "P", "L")) # pair (4,5)


  # results in the follwing pairs:
  #
  # id1 id2 match? link?
  # 1   2   NA     L
  # 1   3   NA     L
  # 1   4   NA     L
  # 1   5   NA     N
  # 2   3   FALSE  N
  # 2   4   FALSE  N
  # 2   5   FALSE  N
  # 3   4   TRUE   L
  # 3   5   TRUE   L
  # 4   5   TRUE   N

  # result should be a table of the form
  #
  #   3 0 0
  #   1 0 3
  #   1 0 2
  #

  checkEqualsNumeric(getTable(result), matrix(c(3,1,1,0,0,0,0,3,2), nrow=3, ncol=3))

  # both possible links and missing identity information
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- c(NA,1,3,3,3)
  rpairs <- compare.dedup(data1, identity = identity1)


  # select pairs as links such there are distinct numbers for false positives etc.
  # leads to 3 TP, 2 FP, 1 FN, 4 TN
  result <- rpairs
  class(result) <- "RecLinkResult"
  result$prediction <- factor(c("P", "L", "L", "N", # pairs (1,x)
         "N", "N", "N", # pairs (2,x)
         "L", "L", # pairs (3,x)
         "N"), levels=c("N", "P", "L")) # pair (4,5)


  # results in the follwing pairs:
  #
  # id1 id2 match? link?
  # 1   2   NA     P
  # 1   3   NA     L
  # 1   4   NA     L
  # 1   5   NA     N
  # 2   3   FALSE  N
  # 2   4   FALSE  N
  # 2   5   FALSE  N
  # 3   4   TRUE   L
  # 3   5   TRUE   L
  # 4   5   TRUE   N

  # result should be a table of the form
  #
  #   3 0 0
  #   1 1 2
  #   1 0 2
  #

  checkEqualsNumeric(getTable(result), matrix(c(3,1,1,0,1,0,0,2,2), nrow=3, ncol=3))

  #
}