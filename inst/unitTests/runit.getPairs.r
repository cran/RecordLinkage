# RUnit tests for functions in getPairs.r

# handling illegal arguments in getPairs()


# empty input set: error or output empty set?

test.getPairs.exceptions <- function()
{
  load("rpairs.em.rda")
  # errors for argument rpairs:

    # illegal type
    checkException(getPairs(rpairs$pairs),
      msg="illegal type for rpairs")
    # wrong class
    rpairs2 <- rpairs
    class(rpairs2) <- "list"
    checkException(getPairs(rpairs2),
      msg="wrong class for rpairs")

  # errors for argument max.weight
  
    # wrong type
    checkException(getPairs(rpairs, max.weight="10"),
      msg="wrong type for max.weight")
    checkException(getPairs(rpairs, max.weight=3i),
      msg="wrong type for max.weight")
  
  # illegal value for argument min.weight

    # wrong type
    checkException(getPairs(rpairs, max.weight="10"),
      msg="wrong type for max.weight")
    checkException(getPairs(rpairs, max.weight=3i),
      msg="wrong type for max.weight")
  
  # no illegal value for single.rows -> use double rows if not FALSE

  # illegal type or value for show
    checkException(getPairs(rpairs, show=1), msg="Illegal type for show")
    checkException(getPairs(rpairs, show="liks"), msg="Illegal value for show")
  
  # no illegal value for sort: no sorting if not identical to TRUE
  
  # illegal threshold combination
    checkException(getPairs(rpairs, max.weight=sample(0:min(rpairs$Wdata),1), 
      min.weight=sample(1:max(rpairs$Wdata),1)), 
      msg="max.weight lower than min.weight")
  

}

test.getPairs <- function()
{
  load("rpairs.em.rda")
  # classification result is needed in some of the tests
  thresh1 <- optimalThreshold(rpairs, my=0.05)
  thresh2 <- optimalThreshold(rpairs, ny=0.05)
  rpairs <- emClassify(rpairs, thresh1, thresh2)

  # use with default arguments and data with weights
  result <- getPairs(rpairs)
  
  # check general format
  checkEquals(colnames(result), c("id",
    colnames(rpairs$pairs[-c(1,2,ncol(rpairs$pairs))]), "Weight"))
  # ensure number of pairs is correct
  checkEquals(nrow(result)/3, nrow(rpairs$pairs),
    msg="check number of pairs for default arguments")
  # ensure pairs are sorted correctly by weight
  weights <- result$Weight[result$Weight!=""]
  weights <- as.numeric(levels(weights)[weights])
  checkEqualsNumeric(sort(weights, decreasing=TRUE), weights, tolerance=1e-6,
    msg="check sorting for default arguments")

  # use with default arguments but with dataset without weights
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  result <- getPairs(rpairs2)
  # ensure number of pairs is correct
  checkEquals(nrow(result)/3, nrow(rpairs$pairs),
    msg="check number of pairs for default arguments and data without weights")
  # check that weights are set to NA
  ind <- 3* (1:(nrow(result)/3)) - 1
  checkTrue(all(is.na(result$Weight[ind])),
    msg="check that weights are NA when using dataset without weights")
  
  # use with thresholds but no link restriction
  
  # do some iterations with different weight limits
  for (i in 1:10)
  {  
    # upper threshold only
    max.weight <- runif(1,0,max(rpairs$Wdata))
    result <- getPairs(rpairs, max.weight=max.weight)
    ind <- 3* (1:(nrow(result)/3)) - 1
    # check that threshold limits are met
    checkTrue(all(as.numeric(as.character(result$Weight[ind])) < max.weight),
      msg="check weight limit through upper threshold")
    # check that number of selected pairs is correct
    checkEquals(nrow(result) / 3, sum(rpairs$Wdata < max.weight),
      msg="check number of pairs with upper threshold")
  
  # lower threshold only
    min.weight <- runif(1,min(rpairs$Wdata),0)
    result <- getPairs(rpairs, min.weight=min.weight)
    ind <- 3* (1:(nrow(result)/3)) - 1
    # check that threshold limits are met
    checkTrue(all(as.numeric(as.character(result$Weight[ind])) >= min.weight),
      msg="check weight limit through lower threshold")
    # check that number of selected pairs is correct
    checkEquals(nrow(result) / 3, sum(rpairs$Wdata >= min.weight),
      msg="check number of pairs with upper threshold")

  # two thresholds
    thresholds <- sample(unique(rpairs$Wdata), 2)
    # extend range to prevent errors due to rounding
    max.weight <- max(thresholds)+0.1
    min.weight <- min(thresholds)-0.1
    result <- getPairs(rpairs, max.weight=max.weight, min.weight=min.weight)
    ind <- 3* (1:(nrow(result)/3)) - 1
    # check that threshold limits are met
    checkTrue(all(as.numeric(as.character(result$Weight[ind])) >= min.weight
      & as.numeric(as.character(result$Weight[ind])) < max.weight),
      msg="check weight limit through lower threshold")
    # check that number of selected pairs is correct
    checkEquals(nrow(result) / 3, sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata < max.weight),
      msg="check number of pairs with upper threshold")
}

  # select only links
  result <- getPairs(rpairs, show="links")
  # check that number of selected pairs equals number of links
  links_rpairs <- sum(rpairs$prediction=="L")
  checkEquals(nrow(result)/3, links_rpairs)

  # select only non-links
  result <- getPairs(rpairs, show="nonlinks")
  # check that number of selected pairs equals number of non-links
  nonlinks_rpairs <- sum(rpairs$prediction=="N")
  checkEquals(nrow(result)/3, nonlinks_rpairs)
  
  # select only possible links
  result <- getPairs(rpairs, show="possible")
  # check that number of selected pairs equals number of possible links  
  possible_rpairs <- sum(rpairs$prediction=="P")
  checkEquals(nrow(result)/3, possible_rpairs)

  # combine weight range and linkage restriction

  for ( i in 1:10)
  {
    # links
    link_weights <- rpairs$Wdata[rpairs$prediction=="L"]
    max.weight <- sample((min(link_weights)+0.1):max(rpairs$Wdata),1)
    min.weight <- sample(min(rpairs$Wdata):(min(link_weights)-0.1),1)
    result <-getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
    show="links")
    # check that number of selected pairs is correct
    checkEquals(nrow(result) / 3, sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight & rpairs$prediction=="L"),
      msg="check combination of thresholds and links only")
  
    # non-links
    nonlink_weights <- rpairs$Wdata[rpairs$prediction=="N"]
    max.weight <- sample((max(nonlink_weights)+0.1):max(rpairs$Wdata),1)
    min.weight <- sample(min(rpairs$Wdata):(max(nonlink_weights)-0.1),1)
    result <-getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
    show="nonlinks")
    # check that number of selected pairs is correct
    checkEquals(nrow(result) / 3, sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight & rpairs$prediction=="N"),
      msg="check combination of thresholds and non-links only")

    # possible links
    possible_weights <- rpairs$Wdata[rpairs$prediction=="P"]
    max.weight <- sample((max(possible_weights)+0.1):max(rpairs$Wdata),1)
    min.weight <- sample(min(rpairs$Wdata):(max(possible_weights)-0.1),1)
    result <-getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
    show="possible")
    # check that number of selected pairs is correct
    checkEquals(nrow(result) / 3, sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight & rpairs$prediction=="P"),
      msg="check combination of thresholds and possible links only")
  }

  # check call with empty result
  # chosse limits that fall between two existing weights
  weights <- sort(rpairs$W)
  ind <- sample(length(weights-1),1)
  diff <- weights[ind+1] - weights[ind]
  max.weight <- weights[ind] + diff*0.6
  min.weight <- weights[ind] + diff*0.3
  result <- getPairs(rpairs,max.weight=max.weight, min.weight=min.weight)
  checkEquals(nrow(result), 0, msg="check size of empty result")
  checkEquals(colnames(result), c("id",
    colnames(rpairs$pairs[-c(1,2,ncol(rpairs$pairs))]), "Weight"),
    msg="check column names of empty result")
  
  
  
##### Repeat checks for single row output #####
  
  
  # use with default arguments and data with weights
  result <- getPairs(rpairs, single.rows=TRUE)

  # check general format
  checkEquals(colnames(result), c(
    "id1", paste(colnames(rpairs$pairs[-c(1,2,ncol(rpairs$pairs))]),"1", sep="."),
    "id2", paste(colnames(rpairs$pairs[-c(1,2,ncol(rpairs$pairs))]),"2", sep="."),
    "Weight"))

  # ensure number of pairs is correct
  checkEquals(nrow(result), nrow(rpairs$pairs),
    msg="check number of pairs for default arguments, single row")

  # ensure pairs are sorted correctly by weight
  checkEqualsNumeric(sort(result$Weight, decreasing=TRUE), result$Weight, 
    tolerance=1e-6, msg="check sorting for default arguments, single row")

  # use with default arguments but with dataset without weights
  rpairs2 <- rpairs
  rpairs2$Wdata <- NULL
  result <- getPairs(rpairs2, single.rows=TRUE)
  # ensure number of pairs is correct
  checkEquals(nrow(result), nrow(rpairs$pairs),
    msg=paste("check number of pairs for default arguments and data without", 
    "weights, single row"))
  # check that weights are set to NA
  checkTrue(all(is.na(result$Weight)),
    msg="check that weights are NA when using dataset without weights")
  
  # use with thresholds but no link restriction
  for (i in 1:10)
  {
    # upper threshold only
    max.weight <- runif(1,0,max(rpairs$Wdata))
    result <- getPairs(rpairs, max.weight=max.weight, single.rows=TRUE)
    # check that threshold limits are met
    checkTrue(all(result$Weight < max.weight),
      msg="check weight limit through upper threshold")
    # check that number of selected pairs is correct
    checkEquals(nrow(result), sum(rpairs$Wdata < max.weight),
      msg="check number of pairs with upper threshold")
  
  # lower threshold only
    min.weight <- runif(1,min(rpairs$Wdata),0)
    result <- getPairs(rpairs, min.weight=min.weight, single.rows=TRUE)
    # check that threshold limits are met
    checkTrue(all(result$Weight >= min.weight),
      msg="check weight limit through lower threshold")
    # check that number of selected pairs is correct
    checkEquals(nrow(result), sum(rpairs$Wdata >= min.weight),
      msg="check number of pairs with upper threshold")

  # two thresholds
    thresholds <- sample(unique(rpairs$Wdata), 2)
    # extend range to prevent errors due to rounding
    max.weight <- max(thresholds)+0.1
    min.weight <- min(thresholds)-0.1
    result <- getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
      single.rows=TRUE)
    # check that threshold limits are met
    checkTrue(all(result$Weight >= min.weight & result$Weight < max.weight),
      msg="check weight limit through lower threshold")
    # check that number of selected pairs is correct
    checkEquals(nrow(result), sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight),
      msg="check number of pairs with upper threshold")
  }
  
  # select only links
  result <- getPairs(rpairs, show="links", single.row=TRUE)
  # check that number of selected pairs equals number of links
  links_rpairs <- sum(rpairs$prediction=="L")
  checkEquals(nrow(result), links_rpairs)

  # select only non-links
  result <- getPairs(rpairs, show="nonlinks", single.row=TRUE)
  # check that number of selected pairs equals number of non-links
  nonlinks_rpairs <- sum(rpairs$prediction=="N")
  checkEquals(nrow(result), nonlinks_rpairs)
  
  # select only possible links
  result <- getPairs(rpairs, show="possible", single.row=TRUE)
  # check that number of selected pairs equals number of possible links  
  possible_rpairs <- sum(rpairs$prediction=="P")
  checkEquals(nrow(result), possible_rpairs)
  
  # link selection and weight restriction
  
  for (i in 1:10)
  {
    # links
    link_weights <- rpairs$Wdata[rpairs$prediction=="L"]
    max.weight <- sample((min(link_weights)+0.1):max(rpairs$Wdata),1)
    min.weight <- sample(min(rpairs$Wdata):(min(link_weights)-0.1),1)
    result <-getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
    show="links", single.rows=TRUE)
    # check that number of selected pairs is correct
    checkEquals(nrow(result), sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight & rpairs$prediction=="L"),
      msg="check combination of thresholds and links only, single row")
  
    # non-links
    nonlink_weights <- rpairs$Wdata[rpairs$prediction=="N"]
    max.weight <- sample((max(nonlink_weights)+0.1):max(rpairs$Wdata),1)
    min.weight <- sample(min(rpairs$Wdata):(max(nonlink_weights)-0.1),1)
    result <-getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
    show="nonlinks", single.rows=TRUE)
    # check that number of selected pairs is correct
    checkEquals(nrow(result), sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight & rpairs$prediction=="N"),
      msg="check combination of thresholds and non-links only, single row")

    # possible links
    possible_weights <- rpairs$Wdata[rpairs$prediction=="P"]
    max.weight <- sample((max(possible_weights)+0.1):max(rpairs$Wdata),1)
    min.weight <- sample(min(rpairs$Wdata):(max(possible_weights)-0.1),1)
    result <-getPairs(rpairs, max.weight=max.weight, min.weight=min.weight,
    show="possible", single.rows=TRUE)
    # check that number of selected pairs is correct
    checkEquals(nrow(result), sum(rpairs$Wdata >= min.weight &
      rpairs$Wdata <max.weight & rpairs$prediction=="P"),
      msg="check combination of thresholds and possible links only, single row")
  }
  
  # check call with empty result
  # chosse limits that fall between two existing weights
  weights <- sort(rpairs$W)
  ind <- sample(length(weights-1),1)
  diff <- weights[ind+1] - weights[ind]
  max.weight <- weights[ind] + diff*0.6
  min.weight <- weights[ind] + diff*0.3
  result <- getPairs(rpairs,max.weight=max.weight, min.weight=min.weight,
    single.rows=TRUE)
  checkEquals(nrow(result), 0, msg="check size of empty result")
  checkEquals(colnames(result), c(
    "id1", paste(colnames(rpairs$pairs[-c(1,2,ncol(rpairs$pairs))]),"1", sep="."),
    "id2", paste(colnames(rpairs$pairs[-c(1,2,ncol(rpairs$pairs))]),"2", sep="."),
    "Weight"),
    msg="check column names of empty result, single row")
  
  # checks for usage with linkage datasets omitted, implementation
  # differs only by one assignement

}
