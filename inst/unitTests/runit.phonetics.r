# Test File for Package Record Linkage
#
# Test Functions in phonetics.r

# pho_h
test.pho_h <- function()
{
  testData1=c("Christian", "Michael", NA_character_)
  result1=c("KRISTIAN", "MICHEL", NA_character_)

  # illegal input
  checkException(pho_h(TRUE)) # wrong data type
  checkException(pho_h(4711)) # wrong data type
  checkException(pho_h(as.list(testData1))) # list not supported
  # NAs should work for character type
  checkIdentical(pho_h(NA_character_), NA_character_)
  # whereas other NAs should throw an exception
  checkException(pho_h(NA_integer_))
  checkException(pho_h(NA_real_))
  checkException(pho_h(NA_complex_))
  checkException(pho_h(NA))
  
  # check test data
  checkEquals(pho_h(testData1), result1)
  # factors should also work, they are converted to character
  checkEquals(pho_h(factor(testData1)), result1)
}

# soundex
test.soundex <- function()
{
  testData1=c("Christian", "Michael", NA_character_)
  result1=c("C623", "M240", NA_character_)

  # illegal input
  checkException(soundex(TRUE)) # wrong data type
  checkException(soundex(4711)) # wrong data type
  checkException(soundex(as.list(testData1))) # list not supported
  # NAs should work for character type
  checkIdentical(soundex(NA_character_), NA_character_)
  # whereas other NAs should throw an exception
  checkException(soundex(NA_integer_))
  checkException(soundex(NA_real_))
  checkException(soundex(NA_complex_))
  checkException(soundex(NA))
  
  # check test data
  checkEquals(soundex(testData1), result1)

}