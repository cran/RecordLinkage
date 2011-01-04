# Test File for Package Record Linkage
#
# Test Functions in strcmp.r

# jarowinkler
test.jarowinkler.exceptions <- function()
{
   testData1=c("Christian", "Michael", "Stefan", "Thomas")
   testData2=c("Kristian",  "Michel", "Stephan", "Tomas")  

   
  # illegal input
  checkException(jarowinkler(TRUE,FALSE)) # wrong data type
  checkException(jarowinkler(4711,4712)) # wrong data type
#  checkException(pho_h(as.list(testData1))) # list not supported

  # NAs of wrong type should throw an exception (other than NA_character_,
  # see below)
  checkException(jarowinkler(NA_integer_, "string"))
  checkException(jarowinkler("string", NA_real_))
  checkException(jarowinkler(NA_complex_, NA_complex))
  checkException(jarowinkler(NA, NA_character))

  # matrices with non-matching dimensions should throw an error
  checkException(jarowinkler(matrix(testData1,2,2),
                             matrix(testData2,4,1)))  

}

test.jarowinkler <- function()
{
   testData1=c("Christian", "Michael", "Stefan", "Thomas")
   testData2=c("Kristian",  "Michel", "Stephan", "Tomas")  

  # NA in at least one of the strings should yield NA_real_
  checkIdentical(jarowinkler("string", NA_character_), NA_real_)
  checkIdentical(jarowinkler(NA_character_, "string"), NA_real_)
  checkIdentical(jarowinkler(NA_character_, NA_character_), NA_real_)
  
  # compare single value with vector
  # numbers are rounded for readability, therefore tolerance is
  # set to an appropriate value
  checkEqualsNumeric(jarowinkler("Christian", testData1),
    c(1.0, 0.4761905, 0.5185185, 0.5), tolerance=1e-7)
  
  # compare two matching vectors
  checkEqualsNumeric(jarowinkler(testData1, testData2),
    c(0.8842593, 0.9714286, 0.8944444, 0.95), tolerance=1e-7)

  # compare two matching vectors
  checkEquals(jarowinkler(matrix(testData1, 2, 2), 
    matrix(testData2, 2, 2)), matrix( c(0.8842593, 0.9714286, 
    0.8944444, 0.95), 2, 2), tolerance=1e-7)
}

test.levenshteinDist.exceptions <- function()
{
   testData1=c("Christian", "Michael", "Stefan", "Thomas")
   testData2=c("Kristian",  "Michel", "Stephan", "Tomas")  

   
  # illegal input
  checkException(levenshteinDist(TRUE,FALSE)) # wrong data type
  checkException(levenshteinDist(4711,4712)) # wrong data type
#  checkException(pho_h(as.list(testData1))) # list not supported

  # NAs of wrong type should throw an exception (other than NA_character_,
  # see below)
  checkException(levenshteinDist(NA_integer_, "string"))
  checkException(levenshteinDist("string", NA_real_))
  checkException(levenshteinDist(NA_complex_, NA_complex))
  checkException(levenshteinDist(NA, NA_character))

  # matrices with non-matching dimensions should throw an error
  checkException(levenshteinDist(matrix(testData1,2,2),
                             matrix(testData2,4,1)))  

}

test.levenshteinDist <- function()
{
   testData1=c("Christian", "Michael", "Stefan", "Thomas")
   testData2=c("Kristian",  "Michel", "Stephan", "Tomas")  

   # NA in at least one of the strings should yield NA_integer
  checkIdentical(levenshteinDist("string", NA_character_), NA_integer_)
  checkIdentical(levenshteinDist(NA_character_, "string"), NA_integer_)
  checkIdentical(levenshteinDist(NA_character_, NA_character_), NA_integer_)
  
  # compare single value with vector
  # numbers are rounded for readability, therefore tolerance is
  # set to an appropriate value
  checkEqualsNumeric(levenshteinDist("Christian", testData1),
    c(0, 8, 7, 7))
  
  # compare two matching vectors
  checkEqualsNumeric(levenshteinDist(testData1, testData2),
    c(2, 1, 2, 1))

  # compare two matching matrices
  checkEquals(levenshteinDist(matrix(testData1, 2, 2), 
    matrix(testData2, 2, 2)), matrix( c(2, 1, 2, 1), 2, 2))


  # Comparison is case-sensitive
  checkEqualsNumeric(levenshteinDist("Christian","Stefan"),7,
    msg="check case-sensitive comparison")
  # this was incorrect due to a bug in PHP 3 sources,
  # switched to PostgreSQL implementation on Mar 8, 2010
  checkEqualsNumeric(levenshteinDist("christian","stefan"),6,
    msg="check case-sensitive comparison")
    
}

test.levenshteinSim.exceptions <- function()
{
   testData1=c("Christian", "Michael", "Stefan", "Thomas")
   testData2=c("Kristian",  "Michel", "Stephan", "Tomas")  

   
  # illegal input
  checkException(levenshteinSim(TRUE,FALSE)) # wrong data type
  checkException(levenshteinSim(4711,4712)) # wrong data type
#  checkException(pho_h(as.list(testData1))) # list not supported

  # NAs of wrong type should throw an exception (other than NA_character_,
  # see below)
  checkException(levenshteinSim(NA_integer_, "string"))
  checkException(levenshteinSim("string", NA_real_))
  checkException(levenshteinSim(NA_complex_, NA_complex))
  checkException(levenshteinSim(NA, NA_character))

  # matrices with non-matching dimensions should throw an error
  checkException(levenshteinSim(matrix(testData1,2,2),
                             matrix(testData2,4,1)))  

}


test.levenshteinSim <- function()
{
   testData1=c("Christian", "Michael", "Stefan", "Thomas")
   testData2=c("Kristian",  "Michel", "Stephan", "Tomas")  

   # NA in at least one of the strings should yield NA_real_
  checkIdentical(levenshteinSim("string", NA_character_), NA_real_)
  checkIdentical(levenshteinSim(NA_character_, "string"), NA_real_)
  checkIdentical(levenshteinSim(NA_character_, NA_character_), NA_real_)
  
  # compare single value with vector
  # numbers are rounded for readability, therefore tolerance is
  # set to an appropriate value
  checkEqualsNumeric(levenshteinSim("Christian", testData1),
    c(1, 1/9, 2/9, 2/9))
  
  # compare two matching vectors
  checkEqualsNumeric(levenshteinSim(testData1, testData2),
    c(7/9, 6/7, 5/7, 5/6))

  # compare two matching matrices
  checkEquals(levenshteinSim(matrix(testData1, 2, 2), 
    matrix(testData2, 2, 2)), matrix( c(7/9, 6/7, 5/7, 5/6), 2, 2))


  # Comparison is case-sensitive
  checkEqualsNumeric(levenshteinSim("Christian","Stefan"), 2/9,
    msg="check case-sensitive comparison")
  # this was incorrect due to a bug in PHP 3 sources,
  # switched to PostgreSQL implementation on Mar 8, 2010
  checkEqualsNumeric(levenshteinSim("christian","stefan"),3/9,
    msg="check case-sensitive comparison")
}