# various tests not related to specific functions

# test behaviour for data with only one matching attribute (was a bug in 0.3.1)
# only check that no error occurs

test.onecolumn <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, exclude=1:6, strcmp=TRUE)
  rpairs <- epiWeights(rpairs)
  rpairs <- emWeights(rpairs)
  
  rpairs <- RLBigDataDedup(RLdata500, exclude=1:6, strcmp=TRUE)
  result <- epiClassify(rpairs, 0.9)
  rpairs <- epiWeights(rpairs)
  rpairs <- emWeights(rpairs)
}