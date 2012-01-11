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
  # ff data structures
  checkEquals(class(org), class(copy), msg = " check that copy has same class")
  slotN <- slotNames(class(org))
  for (s in slotN)
  {
    # the connection should be different in every case
    if (s == "pairs")
    {
      checkEquals(as.data.frame(slot(org, s)),
        as.data.frame(slot(copy, s)),
        msg = sprintf(" check that slot %s is equalin copy", s))
    } else if(s %in% c("Wdata", "WdataInd", "M", "U"))
    {
      checkEquals(as.ram(slot(org, s)), as.ram(slot(copy, s)),
        check.attributes = FALSE,
        msg = sprintf(" check that slot %s differs in copy", s))
    } else
    {
      checkEquals(slot(org, s), slot(copy, s),
        msg = sprintf(" check that slot %s is equal in copy", s))

    }
  }
}

test.clone.RLBigData <- function()
{
  rpairsDedupClone <- clone(rpairsDedup)

  # check equality (by value) via utility function
  compareRLBigData(rpairsDedup, rpairsDedupClone)

  # check that alteration of the copy does not affect the original
  tabBefore <- as.data.frame(rpairsDedup@pairs)
  rpairsDedupClone@pairs[1, "bm"] <- rpairsDedupClone@pairs[1, "bm"] + 1
  tabAfter <- as.data.frame(rpairsDedup@pairs)
  checkEquals(tabBefore, tabAfter, msg = paste(" check that original ffdf",
    "is not affected by change in copy"))
}



# Test for save and load functionality in one function
test.saveLoad.RLBigDataDedup <- function()
{
  # save object
  file <- paste(tempfile(), "zip", sep = ".")
  on.exit(unlink(file))


  saveRLObject(rpairsDedup, file = file)

    # check that file exists
  checkTrue(file.exists(file), msg = " check that file was created on save")

  # reload into different variable
  rpairsDedupReload <- loadRLObject(file)
  
  # compare objects
  compareRLBigData(rpairsDedup, rpairsDedupReload)

}



test.saveLoad.RLBigDataLinkage <- function()
{
  # save object
  file <- paste(tempfile(), "zip", sep = ".")
  on.exit(unlink(file))


  saveRLObject(rpairsLinkage, file = file)

  # check that file exists
  checkTrue(file.exists(file), msg = " check that file was created on save")

  # reload into different variable
  rpairsLinkageReload <- loadRLObject(file)

  # compare objects
  compareRLBigData(rpairsLinkage, rpairsLinkageReload)
}

test.loadRLObject.exceptions <- function()
{
  # check that loading from a non-existing file throws an error
  checkException(loadRLObject(tempfile()), msg = paste(" check that loading",
    "from a non-existing file throws an error"))
}
