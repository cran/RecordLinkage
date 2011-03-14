# Unit Test for class RLBigDataLinkage (constructor and
# internal methods). This test corresponds to the test of compare.linkage()


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

  # shortcut for constructing a RLBigDataLinkage object and retreiving the
  # record pairs
  testResultFun <<- function(...)
  {
    object <- RLBigDataLinkage(...)
    begin(object)
    result <- nextPairs(object, n = -1)
    clear(object)
    return(result)
  }

}

# test all kinds of illegal input
test.RLBigDataLinkage.exceptions <- function()
{


  # illegal dataset
  checkException(testResultFun(data2, "andreas")) # wrong data type
  checkException(testResultFun("andreas", data2)) # wrong data type


  # illegal blocking definition
  checkException(testResultFun(data2, data3, blockfld=TRUE)) # wrong type/value
  checkException(testResultFun(data2, data3, blockfld=list(1,list(4,6)))) # nested list
  checkException(testResultFun(data2, data3, blockfld=-3)) # negative index
  checkException(testResultFun(data2, data3, blockfld=0))
  
  # illegal phonetic definition
  checkException(testResultFun(data2, data3, phonetic=list(1,4))) # list not okay
  checkException(testResultFun(data2, data3, phonetic=-3)) # negative index
  checkException(testResultFun(data2, data3, phonetic=0))
    
  # illegal phonetic function
  checkException(testResultFun(data2, data3, phonetic=TRUE, phonfun=5)) # not a function
  checkException(testResultFun(data2, data3, phonetic=TRUE, phonfun="jarowinkler")) # not a function
  checkException(testResultFun(data2, data3, phonetic=TRUE, 
    phonfun=list(pho_h, soundex))) # neither list...
  checkException(testResultFun(data2, data3, phonetic=TRUE, 
    phonfun=c(pho_h, soundex))) # ...nor vector makes sense
    
  # illegal string comparator definition
  checkException(testResultFun(data2, data3, strcmp=list(1,4))) # list not okay
  checkException(testResultFun(data2, data3, strcmp=-3)) # negative index
  checkException(testResultFun(data2, data3, strcmp=0))

  # illegal string comparison function
  checkException(testResultFun(data2, data3, strcmp=TRUE, phonfun=5)) # not a function
  checkException(testResultFun(data2, data3, strcmp=TRUE, phonfun="jarowinkler")) # not a function
  checkException(testResultFun(data2, data3, strcmp=TRUE, 
    phonfun=list(jarowinkler, levenshteinSim))) # neither list...
  checkException(testResultFun(data2, data3, strcmp=TRUE, 
    phonfun=c(jarowinkler, levenshteinSim))) # ...nor vector makes sense
  # how to test if function returns the right thing?

  # illegal exclude field definition    
  checkException(testResultFun(data2, data3, exlude=list(1,4))) # list not okay
  checkException(testResultFun(data2, data3, exclude=-3)) # negative index
  checkException(testResultFun(data2, data3, exclude=0))

  # illegal identity vector
  checkException(testResultFun(data2, data3,identity=as.list(identity1)))

     
  # combinations of arguments that cause an error

  # non-matching data sets
  checkException(testResultFun(data2, data3[-2]))
  
  # dataset and blocking definition:
  checkException(testResultFun(data2, data3, blockfld=c(4,10))) # out of bounds
  # silently ignored
  checkException(testResultFun(data2, data3, blockfld=c("fname_c1","lname"))) # non-existing column

  # dataset and phonetic:
  checkException(testResultFun(data2, data3, phonetic=c(4,10))) # out of bounds

  # dataset and strcmp:
  checkException(testResultFun(data2, data3, strcmp=c(4,10))) # out of bounds

  # dataset and exclude
  checkException(testResultFun(data2, data3, exclude=c(1,10))) # out of bounds
  
  # dataset and identity vector (illegal length)
  checkException(testResultFun(data2, data3, identity1=1:2, identity2=identity3))
  checkException(testResultFun(data2, data3, identity1=identity2, identity2=1:10))

  # dataset and n_match / n_non_match: see below
  
  # blockfld: no conflicts with other args
  
  # check some cases which cause a warning
  
  # phonetic and strcmp
  # error: string comparator and phonetic code for same column
  # first set warnings to errors
  oldWarn <- getOption("warn")
  options(warn=2)
  checkException(testResultFun(data2, data3, strcmp=1, phonetic=1))  # single column
  checkException(testResultFun(data2, data3, strcmp=1:3, phonetic=3)) # one overlap
  checkException(testResultFun(data2, data3, strcmp=3, phonetic=1:3))  
  checkException(testResultFun(data2, data3, strcmp=1:3, phonetic=2:4)) # several overlaps 
  checkException(testResultFun(data2, data3, strcmp=1:3, phonetic=2:4))  
  checkException(testResultFun(data2, data3, strcmp=TRUE, phonetic=TRUE))  
  checkException(testResultFun(data2, data3, strcmp=1, phonetic=TRUE))  
  checkException(testResultFun(data2, data3, strcmp=TRUE, phonetic=1))  

  # different types for identity1 and identity2 may work but can indicate
  # something is wrong
  checkException(testResultFun(data2, data3, identity1 = identity2, 
    identity2 = as.character(identity3)))
  # reset warning handling
  options(warn=oldWarn)
  
  # strcmp: no conflicts with other args
  # exclude: no conflicts with other args
  # ...
  
}


# test 'normal' behaviour including errors that occur later during execution
test.RLBigDataLinkage <- function()
{

  # no blocking etc.
  testResult=testResultFun(data2,data3) # default case: no blocking whatsoever
  reqResult=read.table("result8.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (no blocking or other options)")

  object <- RLBigDataLinkage(data2, data3)

  # check other components only once
  checkEquals(as.vector(class(object)), "RLBigDataLinkage", msg=" (check class)")
  checkEquals(object@data1, data2, msg=" (check data1 component)")
  checkEquals(object@data2, data3, msg=" (check data2 component)")
  checkEqualsNumeric(getFrequencies(object), frequencies1, tolerance=1e-6,
    msg=" (check frequencies)")

  # same case with usage of identity vector:
  testResult=testResultFun(data2, data3, identity1=identity2,
    identity2=identity3)
  reqResult=read.table("result9.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (check identity vector)")

  # now use blocking on last name  
  # also tests behaviour for result with only one pair
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=3)
  reqResult=read.table("result10.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (blocking on one component)")
  # repeat with textual column id 
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld="lname_c1")
  checkEquals(testResult, reqResult, 
    msg=" (blocking on one component, text id)")
  
   
  # same blocking with phonetic coding yields more results
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=3, phonetic=1:4)
  reqResult=read.table("result11.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (blocking with phonetic code")

  # blocking on two components
  testResult=testResultFun(data2, data3, identity1=identity2,
    identity2=identity3, blockfld=c(5,6))
  reqResult=read.table("result12.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (blocking with two components")
  # repeat with textual column id
  testResult=testResultFun(data2, data3, identity1=identity2,
    identity2=identity3, blockfld=c("by", "bm"))
  checkEquals(testResult, reqResult,
    msg=" (blocking with two components, text id)")

  # combine blocking on data components with blocking on last name
  # (disjunction)
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=list(3,c(5,6)))
  reqResult=read.table("result13.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (combined blocking criteria")
  # repeat with textual column id 
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3,  blockfld=list("lname_c1",c("by", "bm")))
  checkEquals(testResult, reqResult, msg=" (combined blocking criteria")

  # too restrictive blocking (i.e. no resulting pairs) should fail)
  
  checkException(testResultFun(data2, data3, blockfld=1:4),
    msg=" (no record pairs generated)")
  
  # exclude columns, still allow blocking and phonetic code
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3,  blockfld=3, phonetic=1:4, exclude=c(3,4))
  reqResult=read.table("result14.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (exclude columns)")

  object <- RLBigDataLinkage(data2, data3, identity1=identity2, 
    identity2=identity3,  blockfld=3, phonetic=1:4, exclude=c(3,4))
  # check if frequencies are based on used fields
  checkEqualsNumeric(getFrequencies(object), frequencies1[-c(3,4)],
    tolerance = 1e-6, msg = " (check frequencies with excluded attributes)")
  checkEquals(names(getFrequencies(object)), 
    colnames(testResult)[-c(1,2,ncol(testResult))],
    msg = "check names of frequencies with excluded attributes")
  

  # use string comparator for all fields
  # actual values are tested seperately (tests for strcmp.r)
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=3, strcmp=TRUE, strcmpfun="jarowinkler")
  reqResult=c(1,1,jarowinkler( c("FRANK",NA,"MUELLER",NA,1967,9,27), 
    c("MARTIN",NA,"MUELLER",NA,1950,2,4)),0)
  checkEquals(as.double(testResult),reqResult, msg=" (jarowinkler, all fields)")
  
  # string comparison and phonetic code should work although warning is raised
  testResult=testResultFun(data2, data3, identity1=identity2,
  identity2=identity3, blockfld=c(3,5), strcmp=TRUE,
    phonetic=1:4, phonfun="pho_h", strcmpfun="jarowinkler")
  reqResult=c(2,1,
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
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=3, strcmp=1:4, strcmpfun="jarowinkler")
  reqResult=c(1,1,jarowinkler( c("FRANK",NA,"MUELLER",NA), 
    c("MARTIN",NA,"MUELLER",NA)),0,0,0,0)
  checkEquals(as.double(testResult),reqResult, msg=" (jarowinkler, selected fields)")
    
  # same tests for levenshteinSim
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=3, strcmp=TRUE, strcmpfun="levenshtein")
  reqResult=c(1,1,levenshteinSim( c("FRANK",NA,"MUELLER",NA,1967,9,27), 
    c("MARTIN",NA,"MUELLER",NA,1950,2,4)),0)
  checkEquals(as.double(testResult),reqResult, msg=" (levenshtein, all fields)")

  reqResult=c(1,1,levenshteinSim( c("FRANK",NA,"MUELLER",NA), 
    c("MARTIN",NA,"MUELLER",NA)),0,0,0,0)
  testResult=testResultFun(data2, data3, identity1=identity2, 
    identity2=identity3, blockfld=3, strcmp=1:4, strcmpfun="levenshtein")
  checkEquals(as.double(testResult),reqResult, msg=" (levenshtein, selected fields)")


  

  ### various other checks (e.g. for new found bugs) ###
  
  # check if column names are created if not supplied
  data2noNames=as.matrix(data2)
  colnames(data2noNames)=NULL
  testResult=testResultFun(data2noNames, data3)
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
  data5 <- read.table("data5.compare.txt", sep=",", na.strings="",header=TRUE)
  data6 <- read.table("data6.compare.txt", sep=",", na.strings="",header=TRUE)
  testResult <- testResultFun(data5, data6, blockfld=3, phonetic=3)
  reqResult <- read.table("resultNA2.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  checkEquals(testResult, reqResult, msg=" (check reserved words in data)")

  # rownames attached to data should not confuse construction of pairs
  dataRowNames <- data1
  rownames(dataRowNames) <- nrow(data1):1
  testResult <- testResultFun(dataRowNames, data2)
  reqResult <- testResultFun(data1, data2)
  checkEquals(testResult, reqResult,
    msg = "Check that row names are ignored")

  # Check for bug that row names were stored as strings, resulting in an
  # unexpected ordering
  rpairs <- RLBigDataLinkage(data1, data2)
  testResult <- dbGetQuery(rpairs@con, "select row_names as id from data1")
  checkTrue(is.numeric(testResult$id),
    msg = "Check that record ids are stored as numbers")

  testResult <- dbGetQuery(rpairs@con, "select row_names as id from data2")
  checkTrue(is.numeric(testResult$id),
    msg = "Check that record ids are stored as numbers")

  # check that SQL keywords as column names are handled correctly
  # also if column appears in blocking, string comparison or phonetic code
  data1SQLnames <- data1
  colnames(data1SQLnames)=c("fname.c1", "fname.c2", "lname.c1", "lname.c2", "by", "where", "select")

  rpairs <- RLBigDataLinkage(data1SQLnames, data2)
  begin(rpairs)
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names")
  clear(rpairs)

  rpairs <- RLBigDataLinkage(data1SQLnames, data2)
  invisible(begin(rpairs))
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names (in blocking)")
  clear(rpairs)

  rpairs <- RLBigDataLinkage(data1SQLnames, data2)
  invisible(begin(rpairs))
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names (with string comparison)")
  clear(rpairs)

  # in this case
  rpairs <- RLBigDataLinkage(data1SQLnames, data2, phonetic=5:7)
  invisible(begin(rpairs))
  pairs <- nextPairs(rpairs)
  checkEquals(colnames(pairs)[-c(1,2,ncol(pairs))], colnames(data1SQLnames),
    msg = " check SQL keywords as column names (with phonetic code)")
  clear(rpairs)

  # check case when all columns except one are excluded (fix in rev 327)
  testResult=testResultFun(data2,data3, exclude=2:ncol(data2)) # default case: no blocking whatsoever
  reqResult=read.table("result8.compare.txt",sep=",",colClasses="double",
    header=TRUE)
  reqResult <- reqResult[,c(1:3, ncol(reqResult))]
  checkEquals(testResult, reqResult, msg=" (all columns excluded except one)")

}


