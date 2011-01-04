test.mrl <- function()
{
  # exceptions
  checkException(mrl(c("a", "b", "c")))

  test_dat <- c(1,2,3,4,5)
  checkEqualsNumeric(mrl(test_dat, umin=0, umax=4, nint=5)$y,
    c(3, 2.5, 2, 1.5, 1))
}


test.gpdEst <- function()
{
  # exception: no data above threshold
  dat <- rgpd(100)
  checkException(gpdEst(dat, max(dat)+1))

  # exception: wrong data type
  checkException(gpdEst(c("A","B","C")))
  checkException(gpdEst(dat,"A"))


  quantil <- runif(1,0.9,0.99)
  quant <- qgpd(quantil) # calculate quantile for default args (loc=0, scale=1, shape=0)
  X=replicate(300,gpdEst(rgpd(100),0,quantil)) # generate sample with same parameters
  # check that mean value is within 5% of expected value
  checkEqualsNumeric(quant, mean(X), tol = quant * 0.05)
  # check that no value is below threshold
  checkTrue(all(X>=0))
  # check that value is not higher than data
  for (i in 1:100)
  {
    dat <- rgpd(100)
    checkTrue(gpdEst(dat,0,quantil) < max(dat))
  }
}


test.getParetoThreshold.exceptions <- function()
{
    load("rpairs.em.rda")
  # errors concerning rpairs
    # illegal class, type of rpairs
    rpairs2 <- rpairs
    class(rpairs2) <- "wrongClass"
    checkException(getParetoThreshold(rpairs2), msg = "wrong class for rpairs")
    
    rpairs2 <- rpairs$pairs
    class(rpairs2) <- "RecLinkData"
    checkException(getParetoThreshold(rpairs2), msg = "wrong type for rpairs")
    
    # no record pairs
    rpairs2 <- rpairs
    rpairs2$pairs <- rpairs$pairs[0,]
    checkException(getParetoThreshold(rpairs2), msg = "no data pairs")
  
    # no weights in rpairs
    rpairs2 <- rpairs
    rpairs2$Wdata <- NULL
    checkException(getParetoThreshold(rpairs2), msg = "no weights in rpairs")

  # errors concerning quantil
  checkException(getParetoThreshold(rpairs, quantil = "0.1"),
    msg = "illegal type for quantil")
  checkException(getParetoThreshold(rpairs, quantil = TRUE),
    msg = "illegal type for quantil")
  checkException(getParetoThreshold(rpairs, quantil = "0.1"),
    msg = "illegal type for quantil")

  checkException(getParetoThreshold(rpairs, quantil = 1.01),
    msg = "illegal value for quantil")
  checkException(getParetoThreshold(rpairs, quantil = -1),
    msg = "illegal value for quantil")

  # errors concerning interval
  checkException(getParetoThreshold(rpairs, interval=c("a","b")),
    msg = "Wrong type for interval")
  checkException(getParetoThreshold(rpairs, interval=TRUE),
    msg = "Wrong type for interval")
  checkException(getParetoThreshold(rpairs, interval=c(1,1+2i)),
    msg = "Wrong type for interval")

  checkException(getParetoThreshold(rpairs, interval=c(10,-10)),
    msg = "Reversed order in interval")

  # errors concerning rpairs and interval
    # interval out of bounds / no weights in interval
  interval <- c(max(rpairs$Wdata+0.1), max(rpairs$Wdata+20))
  checkException(getParetoThreshold(rpairs, interval=interval))
  interval <- c(min(rpairs$Wdata-20), min(rpairs$Wdata-0.1))
  checkException(getParetoThreshold(rpairs, interval=interval))
}


test.getParetoThreshold <- function()
{
  load("rpairs.em.rda")
  for (i in 1:10)
  {
    # choose some random limits and quantile within a suitable range
    limits <- c(runif(1, -5, 5), runif(1, 20, 25))
    quantil <- runif(1)
    thresh <- getParetoThreshold(rpairs, quantil = quantil, interval = limits)
    checkTrue(thresh >= limits[1] && thresh <= limits[2])
  }  
}

