test.summary.RecLinkData <- function()
{
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- scan("identity1.compare.txt",comment.char="#",sep=",")
  # blocking yields 5 record pairs
  rpairs <-  compare.dedup(data1)
  class(rpairs) <- "RecLinkResult"
  

  # illegal calls
  rpairs2 <- rpairs
  class(rpairs2) <- "illegal"
  checkException(summary.RecLinkData(rpairs2, msg = "Wrong class for rpairs"))

  # normal behavoiur difficult to check automatically
}

test.summary.RecLinkResult <- function()
{
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- scan("identity1.compare.txt",comment.char="#",sep=",")
  # blocking yields 5 record pairs
  rpairs <-  compare.dedup(data1)
  class(rpairs) <- "RecLinkResult"
  

  # illegal calls
  rpairs2 <- rpairs
  class(rpairs2) <- "RecLinkData"
  checkException(summary.RecLinkResult(rpairs2, msg = "Wrong class for rpairs"))
  
  # normal behavoiur difficult to check automatically
}

test.errorMeasures <- function()
{
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- scan("identity1.compare.txt",comment.char="#",sep=",")
  # blocking yields 5 record pairs
  rpairs <-  compare.dedup(data1)
  class(rpairs) <- "RecLinkResult"
  

  # illegal calls
  rpairs2 <- rpairs
  class(rpairs2) <- "RecLinkData"
  checkException(errorMeasures(rpairs2), msg = "Wrong class for rpairs")


  # use data from compare test
  data1 <- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <- scan("identity1.compare.txt",comment.char="#",sep=",")
  # blocking yields 5 record pairs
  rpairs <-  compare.dedup(data1)
  class(rpairs) <- "RecLinkResult"
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
  
}