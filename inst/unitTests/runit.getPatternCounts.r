.setUp <- function()
{
  # data used for the test
  data1 <<- read.table("data1.compare.txt", sep=",", na.strings="",header=TRUE)
  identity1 <<- scan("identity1.compare.txt",comment.char="#",sep=",")
  frequencies1 <<- scan("frequencies1.compare.txt",comment.char="#",sep=",")

  data2 <<- read.table("data2.compare.txt", sep=",", na.strings="",header=TRUE)
  identity2 <<-scan("identity2.compare.txt",comment.char="#",sep=",")
  data3 <<- read.table("data3.compare.txt", sep=",", na.strings="",header=TRUE)
  identity3 <<- scan("identity3.compare.txt",comment.char="#",sep=",")

  # shortcut for constructing a RLBigDataDedup object and retreiving the
  # record pairs
  testResultFun <<- function(...)
  {
    object <- RLBigDataDedup(...)
    begin(object)
    result <- nextPairs(object, n = -1)
    clear(object)
    return(result)
  }

}

test.getPatternCounts <- function()
{
  # Test für Dedup-Objekt
  object <- RLBigDataDedup(data1) # default case: no blocking whatsoever
  result1=read.table("result1.getPatternCounts.txt")
  # Check only numeric equality. Reason: result1 is read as a data frame with
  # one column, which is not easily convertible to a vector with names
  checkEqualsNumeric(getPatternCounts(object), result1[[1]])
  checkEqualsNumeric(getPatternCounts(object,n=1), result1[[1]])
  checkEqualsNumeric(getPatternCounts(object,n=4), result1[[1]])
}
