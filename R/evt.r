# evt.r: Functions for applying Extreme Value Theory to Record Linkage

# simplified version of mrl.plot in package ismev
mrl <- function(data, umin = min(data), umax = max(data) - 0.1, nint = 
	round(max(data)-min(data))*20)
{
#
# function to produce empirical mean residual life plot
# as function of threshold.
	x <- xu <- xl <- numeric(nint)
	u <- seq(umin, umax, length = nint)
	for(i in 1:nint) {
		data <- data[data > u[i]]
		x[i] <- mean(data - u[i])
	}
	return(list(x=u,y=x))
}

# Estimation of quantile in pareto distribution
gpdEst <- function(Wdata, thresh=-Inf, quantil=0.95)
{
    gpd=fpot(x=Wdata, threshold=thresh,std.err=FALSE)
    n=length(Wdata)
    scale=gpd$estimate[1]
    shape=gpd$estimate[2]
   	k=length(gpd$exceedances) # number of exceedances over thresh
    x_quantil=thresh+scale/shape*((n/k*(1-quantil))^(-shape) -1)
    # adjust to reasonable value
    if (x_quantil > -scale/shape) x_quantil <- (-scale/shape)
    if (x_quantil < thresh) x_quantil <- (median(c(thresh,max(Wdata))))
    return (x_quantil)
} 

# ------------------------------------------------------------------------------
# .computeMRL
#
# Internal function to compute the Mean Residual Life (MRL) curve from a vector
# of weights W. This function is optimized for performance using data.table,
# and avoids expensive nested loops by using cumulative sums.
# Many thanks to Vincent Dorie for this contribution
#
# The MRL curve provides the expected excess above a threshold for each 
# distinct value in W, and is useful in fields such as extreme value theory 
# and reliability analysis.
#
# The function returns a list with:
#   - x: positions at which the MRL is evaluated (including shifted epsilon)
#   - y: corresponding mean residual life values
#
# This version assumes W is numeric and may contain repeated values. It groups
# identical weights, computes their frequencies, and builds a histogram for 
# efficient MRL calculation.
#
# Note: This function is not exported and intended for internal use only.
# ------------------------------------------------------------------------------

# Suppress R CMD check notes for data.table column names used with NSE
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("W", "N", "Wcum", "Ncum", "NWcum"))
}

.computeMRL <- function(W) {
  # Convert W into a factor to control sorting order explicitly
  Wtable <- data.table(W = factor(W))
  
  # Force W to be treated as sorted by data.table, without reordering levels
  attr(Wtable, "sorted") <- "W"
  
  # Create a histogram: count how often each weight appears
  histogram <- Wtable[, .N, by = W]
  
  # Convert factor levels back to numeric values for further computation
  histogram[, W := as.numeric(levels(W)[W])]
  
  # Ensure histogram is sorted by weight values in increasing order
  setorder(histogram, W)
  
  # Number of distinct weight values
  nVal <- nrow(histogram)
  
  # Preallocate result vectors (length = 2*nVal - 1 due to interpolation step)
  mrlX <- mrlVal <- numeric(2 * nVal - 1)
  
  # Compute cumulative sums from highest weight downward:
  # - Ncum: cumulative number of values ≥ threshold
  # - NWcum: cumulative weighted sum of values ≥ threshold
  histogram[, `:=`(
    Ncum = rev(cumsum(rev(N))),
    NWcum = rev(cumsum(rev(N * W)))
  )]
  
  # Compute MRL at each W value (odd positions in result vectors)
  i <- seq_len(nVal - 1L)
  mrlVal[2L * i - 1L] <- (
    histogram[i + 1L, NWcum] - histogram[i + 1L, Ncum] * histogram[i, W]
  ) / histogram[i + 1L, Ncum]
  mrlX[2L * i - 1L] <- histogram[i, W]
  
  # Compute MRL just before each W value (even positions), using small epsilon shift
  epsilon <- sqrt(.Machine$double.eps)
  i <- seq.int(2L, nVal)
  thisW <- histogram[i, W] + epsilon  # shifting right to approximate left-side value
  mrlVal[2L * (i - 1L)] <- (
    histogram[i, NWcum] - histogram[i, Ncum] * thisW
  ) / histogram[i, Ncum]
  mrlX[2L * (i - 1L)] <- histogram[i, W] - epsilon
  
  # Set final x-position to the maximum weight (no excess beyond this)
  mrlX[length(mrlX)] <- histogram[nVal, W]
  
  # Return x and y vectors representing the MRL function
  list(x = mrlX, y = mrlVal)
}


# MRL plot
plotMRL <- function(rpairs,l = .computeMRL(sort(as.ram((rpairs$Wdata)))))
{
  plot(l$x,l$y,type="l",lty="blank",xlab="Threshold",ylab="MRL")
  # Draw grid
  abline(v=pretty(extendrange(l$x),n=40),h=pretty(extendrange(l$y),n=40),col="lightgray")
  abline(v=pretty(extendrange(l$x),n=8),h=pretty(extendrange(l$y),n=8),col="gray")
  box()
  points(l$x,l$y,type="l")
}


setGeneric(
  name = "getParetoThreshold",
  def = function(rpairs, quantil=0.95, interval=NA) standardGeneric("getParetoThreshold")
)

setMethod(
  f = "getParetoThreshold",
  signature = "RecLinkData",
  definition = function(rpairs, quantil=0.95, interval=NA)
  {
    if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
      stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

    if (nrow(rpairs$pairs) == 0)
      stop("No record pairs!")

    if (is.null(rpairs$Wdata))
      stop("No weights in rpairs!")

    if (!is.numeric(quantil))
      stop(sprintf("Illegal type for quantil: %s", class(quantil)))

    if (quantil <0 || quantil > 1)
      stop(sprintf("Illegal value for quantil: %g!", quantil))

    if (!missing(interval) && !is.numeric(interval))
      stop(sprintf("Illegal class for interval: %s!", class(interval)))


    # Choose interval in plot
    l=.computeMRL(sort(rpairs$Wdata))
    if (!is.numeric(interval))
    {
      message("Choose interval for pareto distribution")
      flush.console()
      repeat
      {
        plotMRL(NULL,l=l)
  #      title(main=rpairs$description)
        if (existsFunction("bringToTop")) bringToTop()
        indices=sort(identify(l$x,l$y,n=2,labels=signif(l$x,4)))
        interval=l$x[indices]
        if (length(indices)==0)
          stop("At least the left endpoint of the interval must be chosen!")
        if (length(interval)==1)
          interval=c(interval,max(rpairs$Wdata))
        if (any(rpairs$Wdata > interval[1] & rpairs$Wdata <=interval[2])) break
        message("No data in selected range! Choose a larger interval.")
        flush.console()
      }
      if (existsFunction("bringToTop")) bringToTop(-1)
      if (length(indices)==0)
        stop("At least the left endpoint of the interval must be chosen!")
    }
    # If only left limit has been chosen, leave interval open to the right
    fatTail=rpairs$Wdata[rpairs$Wdata <= interval[2]]
    threshold=gpdEst(fatTail,interval[1],quantil)
    return(as.vector(threshold))
  }
)

setMethod(
  f = "getParetoThreshold",
  signature = "RLBigData",
  definition = function(rpairs, quantil=0.95, interval=NA)
  {

    if (nrow(rpairs@pairs) == 0)
      stop("No record pairs!")

    if (is.null(rpairs@Wdata))
      stop("No weights in rpairs!")

    if (!is.numeric(quantil))
      stop(sprintf("Illegal type for quantil: %s", class(quantil)))

    if (quantil <0 || quantil > 1)
      stop(sprintf("Illegal value for quantil: %g!", quantil))

    if (!missing(interval) && !is.numeric(interval))
      stop(sprintf("Illegal class for interval: %s!", class(interval)))


    # Choose interval from plot
    l=.computeMRL(as.ram(ffsort(rpairs@Wdata)))
    if (!is.numeric(interval))
    {
      message("Choose interval for pareto distribution")
      flush.console()
      repeat
      {
        plotMRL(NULL,l=l)
  #      title(main=rpairs$description)
        if (existsFunction("bringToTop")) bringToTop()
        indices=sort(identify(l$x,l$y,n=2,labels=signif(l$x,4)))
        interval=l$x[indices]
        if (length(indices)==0)
          stop("At least the left endpoint of the interval must be chosen!")
        if (length(interval)==1)
          interval=c(interval,max(rpairs$Wdata))
        # determine if there are weights in the selected range
        weightsInRange <- FALSE
        ffvecapply(
          {
            slice <- rpairs@Wdata[i1:i2]
            if (any(slice > interval[1] & slice <= interval[2]))
            {
              weightsInRange <- TRUE
              .break <- TRUE # breaks the ffvecapply loop
            }
          }
          , X = rpairs@Wdata)
        if (weightsInRange) break
        message("No data in selected range! Choose a larger interval.")
        flush.console()
      }
      if (existsFunction("bringToTop")) bringToTop(-1)
      if (length(indices)==0)
        stop("At least the left endpoint of the interval must be chosen!")
    }
    fatTailInd <- ffvecapply(
      {
        slice <- rpairs@Wdata[i1:i2]
        which(slice <= interval[2]) + i1 - 1
      }, X = rpairs@Wdata, RETURN = TRUE, CFUN = "c")
    fatTail=rpairs@Wdata[fatTailInd]
    threshold=gpdEst(fatTail,interval[1],quantil)
    return(as.vector(threshold))
  }
)
