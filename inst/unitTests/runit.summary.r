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


