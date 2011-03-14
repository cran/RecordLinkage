# Unit Test for class RLBigDataDedup (constructor and
# internal methods). This test corresponds to the test of compare.dedup()


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


# test all kinds of illegal input
test.RLBigDataDedup.exceptions <- function()
{

  # illegal dataset
  checkException(RLBigDataDedup("andreas")) # wrong data type

  # illegal blocking definition
  checkException(RLBigDataDedup(data1, blockfld=TRUE)) # wrong type/value
  checkException(RLBigDataDedup(data1, blockfld=list(1,list(4,6)))) # nested list
  checkException(RLBigDataDedup(data1, blockfld=-3)) # negative index
  checkException(RLBigDataDedup(data1, blockfld=0))
  
  # illegal phonetic definition
  checkException(RLBigDataDedup(data1, phonetic=list(1,4))) # list not okay
  checkException(RLBigDataDedup(data1, phonetic=-3)) # negative index
  checkException(RLBigDataDedup(data1, phonetic=0))
    
  # illegal phonetic function
  checkException(RLBigDataDedup(data1, phonetic=TRUE, phonfun=5)) # not a function
  checkException(RLBigDataDedup(data1, phonetic=TRUE, phonfun="jarowinkler"))
  checkException(RLBigDataDedup(data1, phonetic=TRUE, 
    phonfun=list("pho_h", "soundex"))) # neither list...
  checkException(RLBigDataDedup(data1, phonetic=TRUE, 
    phonfun=c("pho_h", "soundex"))) # ...nor vector makes sense

  # illegal string comparator definition
  checkException(RLBigDataDedup(data1, strcmp=list(1,4))) # list not okay
  checkException(RLBigDataDedup(data1, strcmp=-3)) # negative index
  checkException(RLBigDataDedup(data1, strcmp=0))

  # illegal string comparison function
  checkException(RLBigDataDedup(data1, strcmp=TRUE, phonfun=5)) # not a function
  checkException(RLBigDataDedup(data1, strcmp=TRUE, phonfun="jarowinkler")) # not a function
  checkException(RLBigDataDedup(data1, strcmp=TRUE, 
    phonfun=list(jarowinkler, levenshteinSim))) # neither list...
  checkException(RLBigDataDedup(data1, strcmp=TRUE, 
    phonfun=c(jarowinkler, levenshteinSim))) # ...nor vector makes sense

  # illegal exclude field definition    
  checkException(RLBigDataDedup(data1, exclude=list(1,4))) # list not okay
  checkException(RLBigDataDedup(data1, exclude=-3)) # negative index
  checkException(RLBigDataDedup(data1, exclude=0))

  # illegal identity vector
  checkException(RLBigDataDedup(data1,identity=as.list(identity1)))

     
  # combinations of arguments that cause an error

  # dataset and blocking definition:
  checkException(RLBigDataDedup(data1, blockfld=c(4,10))) # out of bounds
# silently ignored:
#  checkException(RLBigDataDedup(data1, blockfld=c("fname_c1","lname"))) # non-existing column

  # dataset and phonetic:
  checkException(RLBigDataDedup(data1, phonetic=c(4,10))) # out of bounds

  # dataset and strcmp:
  checkException(RLBigDataDedup(data1, strcmp=c(4,10))) # out of bounds

  # dataset and exclude
  checkException(RLBigDataDedup(data1, exclude=c(1,10))) # out of bounds
  
  # dataset and identity vector (illegal length)
  checkException(RLBigDataDedup(data1, identity=1:4))

  # dataset and n_match / n_non_match: see below
  
  # blockfld: no conflicts with other args
  
  # phonetic and strcmp
  # error: string comparator and phonetic code for same column
  # first set warnings to errors
  oldWarn <- getOption("warn")
  options(warn=2)
  checkException(RLBigDataDedup(data1, strcmp=1, phonetic=1))  # single column
  checkException(RLBigDataDedup(data1, strcmp=1:3, phonetic=3)) # one overlap
  checkException(RLBigDataDedup(data1, strcmp=3, phonetic=1:3))  
  checkException(RLBigDataDedup(data1, strcmp=1:3, phonetic=2:4)) # several overlaps 
  checkException(RLBigDataDedup(data1, strcmp=1:3, phonetic=2:4))  
  checkException(RLBigDataDedup(data1, strcmp=TRUE, phonetic=TRUE))  
  checkException(RLBigDataDedup(data1, strcmp=1, phonetic=TRUE))  
  checkException(RLBigDataDedup(data1, strcmp=TRUE, phonetic=1))  
  # reset warning handling
  options(warn=oldWarn)
  
  # strcmp: no conflicts with other args
  # exclude: no conflicts with other args
  # ...
  
}

# test 'normal' behaviour including errors that occur later during execution
# only an interface test to check that methods return the expected values,
# no assumptions about internal class structure (slots)
test.RLBigDataDedup <- function()
{

  # no blocking etc.
  testResult=testResultFun(data1) # default case: no blocking whatsoever
  result1=read.table("result1.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, result1, msg=" (no blocking or other options)")

  # check other components only once
  object <- RLBigDataDedup(data1)
  checkEquals(object@data, data1, msg=" (check data component)")
  checkEqualsNumeric(getFrequencies(object), frequencies1, tolerance=1e-6,
    msg=" (check frequencies)")
#

  # same case with usage of identity vector:
  testResult=testResultFun(data1, identity=identity1)
  reqResult=read.table("result2.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (check identity vector)")

  # now use blocking on last name  
  # also tests behaviour for result with only one pair
  testResult=testResultFun(data1, identity=identity1, blockfld=3)
  reqResult=read.table("result3.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (blocking on one component)")
  # repeat with textual column id 
  testResult=testResultFun(data1, identity=identity1, blockfld="lname_c1")
  checkEquals(testResult, reqResult, 
    msg=" (blocking on one component, text id)")
  
   
  # same blocking with phonetic coding yields more results
  testResult=testResultFun(data1, identity=identity1, blockfld=3, phonetic=1:4)
  reqResult=read.table("result4.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (blocking with phonetic code")

  # combined with a too restrictive blocking criterium should yield same result
  # was a bug caused by makeBlockingPairs
  testResult=testResultFun(data1, identity=identity1, blockfld=list(3, 6:7),
   phonetic=1:4)
  checkEquals(testResult, reqResult, msg=" (blocking with phonetic code")
  

  # blocking on two components
  testResult=testResultFun(data1, identity=identity1, blockfld=c(5,6))
  reqResult=read.table("result5.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (blocking with two components")
  # repeat with textual column id 
  testResult=testResultFun(data1, identity=identity1, 
    blockfld=c("by", "bm"))
  checkEquals(testResult, reqResult, 
    msg=" (blocking with two components, text id)")

  # combine blocking on date components with blocking on last name
  # (disjunction)
  testResult=testResultFun(data1, identity=identity1, blockfld=list(3,c(5,6)))
  reqResult=read.table("result6.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (combined blocking criteria)")
  # repeat with textual column id 
  testResult=testResultFun(data1, identity=identity1, blockfld=
  list("lname_c1",c("by", "bm")))
  checkEquals(testResult, reqResult, msg=" (combined blocking criteria)")

  # too restrictive blocking (i.e. no resulting pairs) should fail)
    checkException(testResultFun(data1, blockfld=1:4), msg=" (no record pairs generated)")
  
  # exclude columns, still allow blocking and phonetic code
  testResult=testResultFun(data1, identity=identity1, blockfld=3, phonetic=1:4,
    exclude=c(3,4))
  reqResult=read.table("result7.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (exclude columns)")

  # check frequencies with excluded columns
  object <- RLBigDataDedup(data1, exclude=c(3,4))
  checkEqualsNumeric(getFrequencies(object), frequencies1[-c(3,4)],
    tolerance = 1e-6, msg = " (check frequencies with excluded attributes)")
  checkEquals(names(getFrequencies(object)), 
    colnames(testResult)[-c(1,2,ncol(testResult))],
    msg = "check names of frequencies with excluded attributes")
  

  # use string comparator for all fields
  # actual values are tested seperately (tests for strcmp.r)
  testResult=testResultFun(data1, identity=identity1, blockfld=3, strcmp=TRUE,
    strcmpfun="jarowinkler")
  reqResult=c(1,3,jarowinkler( c("FRANK",NA,"MUELLER",NA,1967,9,27), 
    c("MARTIN",NA,"MUELLER",NA,1950,2,4)),0)
  checkEqualsNumeric(as.matrix(testResult),reqResult, msg=" (jarowinkler, all fields)")

  # string comparison and phonetic code should work although warning is raised
  testResult=testResultFun(data1, identity=identity1, blockfld=c(3,5), strcmp=TRUE,
    phonetic=1:4, phonfun="pho_h", strcmpfun="jarowinkler")
  reqResult=c(1,4,
    jarowinkler(
      c(
        pho_h(
          c("FRANK",NA,"MUELLER",NA)
        ),
        1967,9,27
      ),
      c(
        pho_h(
          c("FRANK","MARTIN","MUELER",NA)
        ),
        1967,8,27
      )
    ),
  1)
  checkEquals(as.double(testResult),reqResult, msg=" (jarowinkler, all fields)")


  # string comparator for individual fields
  reqResult=c(1,3,jarowinkler( c("FRANK",NA,"MUELLER",NA), 
    c("MARTIN",NA,"MUELLER",NA)),0,0,0,0)
  testResult=testResultFun(data1, identity=identity1, blockfld=3, strcmp=1:4,
    strcmpfun="jarowinkler")
  checkEqualsNumeric(as.matrix(testResult),reqResult, msg=" (jarowinkler, selected fields)")

  # TODO: überprüfen, Werte weichen ab!
   # same tests for levenshteinSim
  testResult=testResultFun(data1, identity=identity1, blockfld=3, strcmp=TRUE,
    strcmpfun="levenshtein")
  reqResult=c(1,3,levenshteinSim( c("FRANK",NA,"MUELLER",NA,1967,9,27), 
    c("MARTIN",NA,"MUELLER",NA,1950,2,4)),0)
  checkEqualsNumeric(as.matrix(testResult),reqResult, msg=" (levenshteinSim, all fields)")

  reqResult=c(1,3,levenshteinSim( c("FRANK",NA,"MUELLER",NA), 
    c("MARTIN",NA,"MUELLER",NA)),0,0,0,0)
  testResult=testResultFun(data1, identity=identity1, blockfld=3, strcmp=1:4,
    strcmpfun="levenshtein")
  checkEqualsNumeric(as.matrix(testResult),reqResult, msg=" (levenshteinSim, selected fields)")


  

  ### various other checks (e.g. for new found bugs) ###
  
  # check that column names are created if not supplied
  # also check that matrix can be handled (conversion to data frame)
  data1noNames=as.matrix(data1)
  colnames(data1noNames)=NULL
  testResult=testResultFun(data1noNames)
  resultNames=colnames(testResult)
  checkTrue(!any(is.na(resultNames)), msg=" (check column names)") # no NAs
  checkEquals(anyDuplicated(resultNames),0, 
    msg=" (check column names)")            # uniqueness
  checkEquals(resultNames[1:2],c("id1","id2"), 
    msg=" (check column names)")            # correct id names
  checkEquals(tail(resultNames,1),"is_match",
    msg=" (check column names)")            # correct name for match status

  # test for a bug that occured when "NA" was part of a name, check also
  # for "NULL"
  data4 <- read.table("data4.compare.txt", sep=",", na.strings="",header=TRUE)
  testResult <- testResultFun(data4, blockfld=3, phonetic=3)
  reqResult <- read.table("resultNA.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (check reserved words in data)")
    
  # rownames attached to data should not confuse construction of pairs
  dataRowNames <- data1
  rownames(dataRowNames) <- nrow(data1):1
  testResult <- testResultFun(dataRowNames)
  reqResult <- testResultFun(data1)
  checkEquals(testResult, reqResult,
    msg = "Check that row names are ignored")
    
  # Check for bug that row names were stored as strings, resulting in an
  # unexpected ordering
  rpairs <- RLBigDataDedup(data1)
  testResult <- dbGetQuery(rpairs@con, "select row_names as id from data")
  checkTrue(is.numeric(testResult$id),
    msg = "Check that record ids are stored as numbers")

  # check that SQL keywords as column names are handled correctly
  # also if column appears in blocking, string comparison or phonetic code
  data1SQLnames <- data1
  colnames(data1SQLnames)=c("fname.c1", "fname.c2", "lname.c1", "lname.c2", "by", "where", "select")

  rpairs <- RLBigDataDedup(data1SQLnames)
  begin(rpairs)
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names")
  clear(rpairs)

  rpairs <- RLBigDataDedup(data1SQLnames, blockfld=list(5,6,7))
  invisible(begin(rpairs))
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names (in blocking)")
  clear(rpairs)

  rpairs <- RLBigDataDedup(data1SQLnames, strcmp=5:7)
  invisible(begin(rpairs))
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names (with string comparison)")
  clear(rpairs)

  # in this case
  rpairs <- RLBigDataDedup(data1SQLnames, phonetic=5:7)
  invisible(begin(rpairs))
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names (with phonetic code)")
  clear(rpairs)

  # check case when all columns except one are excluded (fix in rev 327)
  testResult=testResultFun(data1, exclude=2:ncol(data1))
  result1=read.table("result1.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  result1 <- result1[,c(1:3, ncol(result1))]
#  write.table(result1, file="tab1.txt")
#  write.table(testResult, file="tab2.txt")
  checkEquals(testResult, result1, msg=" (all columns excluded except one)")

}

# names of datasets differ?


