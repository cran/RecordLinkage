# epilink.r: Functions for the Epilink matching procedure
# See Continiero et al.: The EpiLink record linkage software, in:
# Methods of Information in Medicine 2005, 44(1):66-71.


epiWeights <- function (rpairs, e=0.01, f=rpairs$frequencies)
{
  # check for erronous input
  
  if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
    stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

  if (nrow(rpairs$pairs) == 0)
    stop("No record pairs!")

  if (!is.numeric(f))
    stop(sprintf("Illegal type for f: %s", class(f)))
  
  if (any(f <=0 | f > 1))
    stop(sprintf("Illegal value for f: %s!", paste(f, collapse=", ")))

  if (!is.numeric(e))
    stop(sprintf("Illegal type for e: %s", class(f)))

  if (any(e <0 | e >= 1))
    stop(sprintf("Illegal value for e: %s!", paste(e, collapse=", ")))

  # check condition e <= 1-f, otherwise illegal weights can occur
  if(any(e > 1-f))
    stop("Condition e <= 1-f does not hold, adjust error rate!")

  # leave out ids and matching status
  pairs=rpairs$pairs[,-c(1,2,ncol(rpairs$pairs))]
  pairs[is.na(pairs)]=0

  # dummy operation to achieve recycling of values
  e=e+rep(0,ncol(pairs))
  f=f+rep(0,ncol(pairs))
  # adjust error rate 
  # error rate
  w=log((1-e)/f, base=2)
  # 
  
  
  # weight computation
  row_sum <- function(r,w)
  {
  return(sum(r*w,na.rm=TRUE))
  }
  
  S=apply(pairs,1,row_sum,w)/sum(w)
  if (any(is.na(S) | S < 0 | S > 1))
    warning("Some weights have illegal values. Check error rate and frequencies!")
  rpairs$Wdata=S
  return(rpairs)
}







# Arguments:
#   rpairs      weighted record pairs (output of emWeights)
#   my      error bound: # False Matches / # Found Matches
#   ny      error bound  # False Non-Matches / # Found Non-Matches
#       If an error bound is Inf, it will not be considered, meaning that
#       "possible link" will not be assigned
epiClassify <- function (rpairs,threshold.upper, 
                        threshold.lower=threshold.upper)
{    

  if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
    stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

  if (nrow(rpairs$pairs) == 0)
    stop("No record pairs!")

  if (is.null(rpairs$Wdata))
    stop("No weights in rpairs!")

  if (!is.numeric(threshold.upper))
    stop(sprintf("Illegal type for threshold.upper: %s", class(threshold.upper)))

  if (!is.numeric(threshold.lower))
    stop(sprintf("Illegal type for threshold.lower: %s", class(threshold.lower)))

  if (threshold.upper < threshold.lower)
    stop(sprintf("Upper threshold %g lower than lower threshold %g",

  threshold.upper, threshold.lower))
  prediction=rep("P",nrow(rpairs$pairs))
  prediction[rpairs$Wdata>=threshold.upper]="L"
  prediction[rpairs$Wdata<threshold.lower]="N"
  
  ret=rpairs # keeps all components of rpairs
  ret$prediction=factor(prediction,levels=c("N","P","L"))
  ret$threshold=threshold.upper
  class(ret)="RecLinkResult"
  return(ret)
}

