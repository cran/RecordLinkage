# em.r: Functions for Record Linkage with weights calculated by the EM algorithm

# Arguments:
#
#   rpairs  data pairs (class RecLinkPairs)
#   m       probability for an error (m-probability), either one value for
#           all attributes or a vector with distinct values
emWeights <- function (rpairs, cutoff=0.95,...)
{
  # check for erronous input
  
  if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
    stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

  if (nrow(rpairs$pairs) == 0)
    stop("No record pairs!")
    
  if (!is.numeric(cutoff))
    stop(sprintf("Illegal type for cutoff: %s", class(cutoff)))
  if (cutoff < 0 || cutoff > 1)
    stop(sprintf("Illegal value for cutoff: %g", cutoff))


  pairs=rpairs$pairs
  # ids und Matchingstatus rausnehmen
  pairs=pairs[,-c(1,2,ncol(pairs))]
  pairs=as.matrix(pairs)
  pairs[is.na(pairs)]=0
  is_fuzzy=!all(is.element(pairs,0:1))
  if (is_fuzzy)
  {
      if(length(cutoff)==1 || length(cutoff)==ncol(pairs)){
      	pairs_fuzzy=pairs
      	pairs=as.array((pairs>=cutoff)*1)
	}
	else {
	 stop("Cutoff must be a vector with length equal to the number of attributes or to one!")
	 }
  }

  n_data=nrow(pairs)  
  observed_count=countpattern(pairs)
  n_attr=ncol(pairs)
  patterns=bincombinations(n_attr)  # Liste der Patterns
  x=c(rep(0,nrow(patterns)),rep(1,nrow(patterns)))
  s=c(1:length(observed_count), 1:length(observed_count))
  i=rep(1,nrow(patterns)) # Intercept
  X=cbind(i,x,rbind(patterns,patterns),rbind(patterns,patterns)*x) # Design Matrix

  u=rpairs$frequencies    
  m=0.97
  # Ad-hoc-Schätzung für Anteil an Matchen (Faktor 0.1 relativ beliebig)
  prob_M=1/sqrt(n_data)*0.1
  # Anzahl schätzen für Matche
  init_M=apply(patterns,1,function(a) prod(a*m+(1-a)*(1-m))*n_data*prob_M)
  init_U=apply(patterns,1,function(a) prod(a*u+(1-a)*(1-u))*n_data*(1-prob_M))
  expected_count=c(init_U,init_M)

  res=mygllm(observed_count,s,X,E=expected_count,...)

  n_patterns=length(res)/2

  # Anteil Matche/Non_Matche in einem Pattern
  matchrate=res[(n_patterns+1):(2*n_patterns)]/res[1:n_patterns]
  #matchrate=round(res[(n_patterns+1):(2*n_patterns)])/round(res[1:n_patterns])
#    o=order(matchrate,res[(n_patterns+1):(2*n_patterns)],decreasing=T)

  n_matches=sum(res[(n_patterns+1):(2*n_patterns)])
  n_nonmatches=sum(res[1:n_patterns])
  U=res[1:n_patterns]/n_nonmatches
  M=res[(n_patterns+1):(2*n_patterns)]/n_matches
  W=log(M/U, base=2)
  indices=colSums(t(pairs)*(2^(n_attr:1-1)))+1    
  ret=rpairs # keeps all components of rpairs
  ret$M=M
  ret$U=U
  ret$W=W
  ret$Wdata=W[indices]
  if (is_fuzzy)
  {
      str_weights=apply(pairs_fuzzy^pairs,1,prod)
      ret$Wdata=ret$Wdata+log(str_weights, base=2)
  } 
  cat("\n")
  return(ret)
}


# Arguments:
#   rpairs      weighted record pairs (output of emWeights)
#   my      error bound: # False Matches / # Found Matches
#   ny      error bound  # False Non-Matches / # Found Non-Matches
#       If an error bound is Inf, it will not be considered, meaning that
#       "possible link" will not be assigned
emClassify <- function (rpairs,threshold.upper=Inf, 
                        threshold.lower=threshold.upper,my=Inf, ny=Inf)
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
      
  if (!is.numeric(my))
    stop(sprintf("Illegal type for my: %s", class(my)))
  if (!missing(my) && (my < 0 || my > 1))
    stop(sprintf("Illegal value for my: %g", my))

  if (!is.numeric(ny))
    stop(sprintf("Illegal type for ny: %s", class(ny)))
  if (!missing(ny) && (ny < 0 || ny > 1))
    stop(sprintf("Illegal value for ny: %g", ny))

  # if no threshold was given, compute them according to the error bounds
  if (missing(threshold.upper) && missing(threshold.lower))
  {
    o=order(rpairs$W,decreasing=TRUE) # order Weights decreasing
    FN=rev(cumsum(rev(rpairs$M[o]))) 
    FP=cumsum(rpairs$U[o])
    if (my==Inf && ny==Inf)
    {
        # no error bound given: minimize overall error
        cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
        if (length(cutoff_upper)==0)
            cutoff_upper=0
        cutoff_lower=cutoff_upper
        
    } else if (my==Inf)
    {  
        # only rate of false matches relevant
        cutoff_lower=head(which(FN<=ny),1)
        if (length(cutoff_lower)==0)
            cutoff_lower=length(o)
        cutoff_upper=cutoff_lower
    
    } else if (ny==Inf)
    {
        # only rate of false non-matches relevant
        cutoff_upper=tail(which(FP<=my),1)
        cutoff_lower=cutoff_upper
    } else
    {
        # both error bounds relevant
        cutoff_upper=tail(which(FP<=my),1)
        cutoff_lower=head(which(FN<=ny),1)
        if (length(cutoff_upper)==0)
            cutoff_upper=0
        if (length(cutoff_lower)==0)
            cutoff_lower=length(o)
        if (cutoff_lower<cutoff_upper)
        {
            cutoff_upper=which.min(c(0,FP)+c(FN,0))-1
            cutoff_lower=cutoff_upper
        }
    } 
    print("Threshold berechnen und Klassifikation zuweisen")
    threshold.upper=rpairs$W[o][cutoff_upper]
    threshold.lower=rpairs$W[o][cutoff_lower]
  } # end if
   
  prediction=rep("P",nrow(rpairs$pairs))
  prediction[rpairs$Wdata>=threshold.upper]="L"
  prediction[rpairs$Wdata<threshold.lower]="N"
  
  ret=rpairs # keeps all components of rpairs
  ret$prediction=factor(prediction,levels=c("N","P","L"))
	ret$threshold=threshold.upper
  class(ret)="RecLinkResult"
  return(ret)
}


optimalThreshold <- function (rpairs, my=NaN, ny=NaN)
{
  if (!("RecLinkData" %in% class(rpairs) || "RecLinkResult" %in% class(rpairs)))
    stop(sprintf("Wrong class for rpairs: %s", class(rpairs)))

  if (nrow(rpairs$pairs) == 0)
    stop("No record pairs!")
  
  if (is.null(rpairs$Wdata))
    stop("No weights in rpairs!")

  if (!is.numeric(my))
    stop(sprintf("Illegal type for my: %s", class(my)))
  if (!missing(my) && (my < 0 || my > 1))
    stop(sprintf("Illegal value for my: %g", my))

  if (!is.numeric(ny))
    stop(sprintf("Illegal type for ny: %s", class(ny)))
  if (!missing(ny) && (ny < 0 || ny > 1))
    stop(sprintf("Illegal value for ny: %g", ny))

	o=order(rpairs$Wdata,decreasing=TRUE)
	weights=rpairs$Wdata[o]
  n_data=length(weights)
	is_match=rpairs$pairs$is_match[o]
	n_match <- sum(is_match==1, na.rm = TRUE)
	n_non_match <- sum(is_match==0, na.rm = TRUE)
	FP_err=cumsum(is_match!=1)#/as.numeric(1:n_data)
  FN_err=rev(cumsum(rev(is_match==1)))#/as.numeric(1:n_data)))
  error=FP_err+FN_err

  # nun baue Tabelle, in der Gewicht (unique) und Fehlerrate gegenübergestellt
  # sind. Die Fehlerrate eines Gewichts ist in der sortierten Tabelle gleich
  # der Fehlerrate für den letzten Datensatz des Blocks
  # tapply() sortiert aufsteigend, das rev() stellt 
  # die absteigende Reihenfolge wieder her

  error_unique=rev(tapply(error,weights,tail,1))
  FP_err_unique=rev(tapply(FP_err/n_non_match,weights,tail,1))
  FN_err_unique=rev(tapply(FN_err/n_match,weights,tail,1))
  
  weights_unique=unique(weights)

  # Bestimme Gewicht des Datensatzes mit minimalem Fehler.

  if (missing(my) && missing(ny))
    return(as.numeric(weights_unique[which.min(error_unique)]))

  if (!missing(my))
  {
    min_ind <- which.min(FN_err_unique[FP_err_unique<=my])
    return(as.numeric(weights_unique[FP_err_unique<=my][min_ind]))
  }

  if (!missing(ny))
  {
    min_ind <- which.min(FP_err_unique[FN_err_unique<=ny])
    return(as.numeric(weights_unique[FN_err_unique<=ny][min_ind]))
  }
}



