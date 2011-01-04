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
