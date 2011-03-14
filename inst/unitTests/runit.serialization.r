.setUp <- function()
{
  data(RLdata500)
  rpairsDedup <<- RLBigDataDedup(RLdata500, identity = identity.RLdata500,
    blockfld = c(1,3))
  # use a subsample of RLdata500 as second data set in a linkage procedure
  s <- sample(500, 200)
  rpairsLinkage <<- RLBigDataLinkage(RLdata500, RLdata500[s,],
    identity1 = identity.RLdata500, identity2 = identity.RLdata500[s],
    blockfld = 1)
}

# utility function to compare an object with its clone
# if samefile=TRUE, check for a copy with the same database file
# (which is created by calling saveRLObject() with argument file missing)
compareRLBigData <- function(org, copy, samefile=FALSE)
{
  # check if all slots of the copy have the same value except the
  # database connection and file
  checkEquals(class(org), class(copy), msg = " check that copy has same class")
  slotN <- slotNames(class(org))
  for (s in slotN)
  {
    # the connection should be different in every case
    if (s == "con")
    {
      checkTrue(!identical(slot(org, s), slot(copy, s)),
        msg = sprintf(" check that slot %s differs in copy", s))
    } else if(s == "dbFile" && !samefile)
    {
      checkTrue(!identical(slot(org, s), slot(copy, s)),
        msg = sprintf(" check that slot %s differs in copy", s))
    } else if(s == "dbFile" && samefile)
    {
      checkEquals(slot(org, s), slot(copy, s),
        msg = sprintf(" check that slot %s is equal in copy", s))
    }else if (s=="drv")
    {
      # no all.equal method for external pointers - use identical instead
      checkTrue(identical(slot(org, s), slot(copy, s)),
        msg = sprintf(" check that slot %s is equal in copy", s))
    } else
    {
      checkEquals(slot(org, s), slot(copy, s),
        msg = sprintf(" check that slot %s is equal in copy", s))

    }
  }

  # check that databases have the same content after copying
  for (tab in dbListTables(org@con))
  {
    # "serialization" may not exist in a copy
    if (tab!="serialization")
    {
      tabOrg <- dbReadTable(org@con, tab)
      tabClone <- dbReadTable(copy@con, tab)
      checkEquals(tabOrg, tabClone,
        msg = sprintf(" check that table %s has same content in copy", tab))
    }
  }

  # check that extension functions have been copied and return the same value
  query <- paste("select jarowinkler('string', 'sting'),",
                 "       levenshtein('string', 'sting'),",
                 "       pho_h('zeichenkette')")

  checkEquals(dbGetQuery(org@con, query), dbGetQuery(copy@con, query),
    msg = " check extension functions in copy")
}

test.clone.RLBigDataDedup <- function()
{
  rpairsDedupClone <- clone(rpairsDedup)

  # check equality (by value) via utility function
  compareRLBigData(rpairsDedup, rpairsDedupClone)

  # check that alteration of the copy does not affect the original
  tabBefore <- dbReadTable(rpairsDedup@con, "data")
  dbGetQuery(rpairsDedupClone@con, "update data set 'bm'='bm' + 1")
  tabAfter <- dbReadTable(rpairsDedup@con, "data")
  checkEquals(tabBefore, tabAfter, msg = paste(" check that original database",
    "is not affected by change in copy"))
}


test.clone.RLBigDataLinkage <- function()
{
  rpairsLinkageClone <- clone(rpairsLinkage)

  # check equality (by value) via utility function
  compareRLBigData(rpairsLinkage, rpairsLinkageClone)


  # check that alteration of the copy does not affect the original
  tabBefore <- dbReadTable(rpairsLinkage@con, "data1")
  dbGetQuery(rpairsLinkageClone@con, "update data1 set 'bm'='bm' + 1")
  tabAfter <- dbReadTable(rpairsLinkage@con, "data1")
  checkEquals(tabBefore, tabAfter, msg = paste(" check that original database",
    "is not affected by change in copy"))
}

# Test for save and load functionality in one function
test.saveLoad.RLBigDataDedup <- function()
{
  # save object
  file <- tempfile()
  on.exit(unlink(file))


  saveRLObject(rpairsDedup, file = file)

    # check that file exists
  checkTrue(file.exists(file), msg = " check that file was created on save")

  # reload into different variable
  rpairsDedupReload <- loadRLObject(file)
  
  # compare objects
  compareRLBigData(rpairsDedup, rpairsDedupReload)

  # save file without filename specified and reload
  saveRLObject(rpairsDedup)

  # reload into different variable
  rpairsDedupReload <- loadRLObject(rpairsDedup@dbFile)

  # compare objects
  compareRLBigData(rpairsDedup, rpairsDedupReload)

  # reload "in place" -> files should be equal
  rpairsDedupReload <- loadRLObject(rpairsDedup@dbFile, inPlace=TRUE)
  compareRLBigData(rpairsDedup, rpairsDedupReload, samefile=TRUE)

}



test.saveLoad.RLBigDataLinkage <- function()
{
  # save object
  file <- tempfile()
  on.exit(unlink(file))


  saveRLObject(rpairsLinkage, file = file)

  # check that file exists
  checkTrue(file.exists(file), msg = " check that file was created on save")

  # reload into different variable
  rpairsLinkageReload <- loadRLObject(file)

  # compare objects
  compareRLBigData(rpairsLinkage, rpairsLinkageReload)

  # save file without filename specified and reload
  saveRLObject(rpairsLinkage)

  # reload into different variable
  rpairsLinkageReload <- loadRLObject(rpairsLinkage@dbFile)

  # compare objects
  compareRLBigData(rpairsLinkage, rpairsLinkageReload)
  
  # reload "in place" -> files should be equal
  rpairsLinkageReload <- loadRLObject(rpairsLinkage@dbFile, inPlace=TRUE)
  compareRLBigData(rpairsLinkage, rpairsLinkageReload, samefile=TRUE)
}

test.loadRLObject.exceptions <- function()
{
  # check that loading from a non-existing file throws an error
  checkException(loadRLObject(tempfile()), msg = paste(" check that loading",
    "from a non-existing file throws an error"))
}
