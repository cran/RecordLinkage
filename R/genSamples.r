# genSamples.r: functions to extract subsets of data sets 
# (e.g. for training sets)

genSamples = function (dataset, num.non, des.mprop=0.1)
{   
  # catch erronous input
  # for dataset: errors are detected in classifyUnsup
  
  if (!is.numeric(num.non))
    stop("Illegal type for num.non!")

  if (!is.numeric(des.mprop))
    stop("Illegal type for des.mprop!")

  if (num.non <0)
    stop(sprintf("Illegal value for num.non: %g", num.non))

  if (des.mprop <= 0)
    stop(sprintf("Illegal value for des.mprop: %g", num.non))

	
    # run clustering algorithm?
    # res_clust <- bclust(x=ddmat, centers=3,base.center=2, base.method="kmeans", iter.base=100, verbose=TRUE)
    # kmendundet<-kmeans(x=ddmat, centers=nclasses); pairs$pairs[kclusr$cluster==2,]
    # Use EM algorithm instead    
#     ddpairs <- emWeights(datapairs)
# 	ddclass <- emClassify(ddpairs)
	ddclass=classifyUnsup(dataset,method="bclust")    
  return(splitData(ddclass, use.pred=TRUE, num.non=num.non, des.mprop=des.mprop))
}
    
    


splitData <- function(dataset, prop=0.5, keep.mprop=FALSE, num.non=0,
  des.mprop=0, use.pred=FALSE)
{
  # catch erronous input
  if (!(("RecLinkData" %in% class(dataset)) || 
    "RecLinkResult" %in% class(dataset)))
    stop("Wrong class for dataset!")
  
  if (nrow(dataset$pairs) < 2)
    stop("Not enough data!")
  
  if (prop <= 0 || prop >= 1)
    stop(sprintf("Illegal value for prop: %g!", prop))

  if (num.non <0)
    stop(sprintf("Illegal value for num.non: %g", num.non))

  if (!is.numeric(num.non))
    stop(sprintf("Illegal type of num.non: %s", typeof(num.non)))

	train=dataset
	valid=dataset
  pairs=dataset$pairs
	n_data=nrow(pairs)
	pairs$is_match=as.logical(pairs$is_match)
	

	if (use.pred)
	{
		if (is.null(dataset$prediction))
			stop ("No prediction vector in dataset! Call with use.pred=FALSE.")
		pairs$is_match=dataset$prediction=="L"
	}

	if (!missing(num.non) && !missing(des.mprop))
	{
   	 	#ids=as.matrix(ret$pairs[,c(1:2)]) # any filters will be set
    	ids <- seq(from=1,to=n_data)
	    nlink <- round(des.mprop*(num.non)) 
	    ngesamt <- num.non+nlink 
	    if (ngesamt > n_data)
			stop("Inconsistent values for training data!")
	    if (des.mprop<0 || des.mprop >=1)
			stop("Inconsistent value for link proportion!")		

    	linksid <- ids[pairs$is_match]
    	nonlinksid <- ids[!pairs$is_match]
    
	    alidn <- length(linksid)
   		anolidn <- length(nonlinksid)

	    nmark=FALSE; mmark=FALSE;
    	if(nlink > alidn) { warning("Only ", alidn, " Links!"); nlink=alidn; nmark=TRUE }
    	if(num.non > anolidn) { warning("Only ", anolidn, " Non-Links!"); num.non=anolidn; mmark=TRUE }
    	salid <- resample(linksid, size=nlink)
    	sanolid <- resample(nonlinksid, size=num.non)
    
	    train=dataset
    	valid=dataset
    	trainid <- c(salid,sanolid)
    	trainhelp <- ids %in% trainid
    	linkhelp <- ids %in% salid
    	nonlinkhelp <- ids %in% sanolid
	    valid$pairs <- dataset$pairs[!trainhelp,]
    	train$pairs <- dataset$pairs[trainhelp,] 
		
		# split prediction vector, if present
		if (!is.null(dataset$prediction))
		{
			valid$prediction <- dataset$prediction[!trainhelp]
			train$prediction <- dataset$prediction[trainhelp]
		}
							  						
		# split weight vector, if present
		if (!is.null(dataset$Wdata))
		{
			valid$Wdata=dataset$Wdata[!trainhelp]
			train$Wdata=dataset$Wdata[trainhelp]

			valid$M=dataset$M
			valid$U=dataset$U
			valid$W=dataset$W
			train$M=dataset$M
			train$U=dataset$U
			train$W=dataset$W
		}
    	return(list(train=train,valid=valid))
	}

	if (isFALSE(keep.mprop))
	{
		s=sample(1:n_data,n_data*prop)
		train$pairs=dataset$pairs[s,]
		valid$pairs=dataset$pairs[-s,]

		# split prediction vector, if present
		if (!is.null(dataset$prediction))
		{
			train$prediction <- dataset$prediction[s]
			valid$prediction <- dataset$prediction[-s]
		}
							  						
		# split weight vector, if present
		if (!is.null(dataset$Wdata))
		{
			train$Wdata=dataset$Wdata[s]
			valid$Wdata=dataset$Wdata[-s]
			valid$M=dataset$M
			valid$U=dataset$U
			valid$W=dataset$W
			train$M=dataset$M
			train$U=dataset$U
			train$W=dataset$W
		}

		return (list(train=train, valid=valid))
	}
	
	if (isTRUE(keep.mprop))
	{
		match_ind=which(as.logical(pairs$is_match))
		n_match=length(match_ind)
		if (n_match==0)
			stop("No matches found! Call with keep.mprop=FALSE.")
		s_match=sample(1:n_match,round(n_match*prop))
		s_non_match=sample(1:(n_data-n_match),round(n_data*prop)-round(n_match*prop))
		train$pairs=rbind(dataset$pairs[match_ind[s_match],],
						  dataset$pairs[-match_ind,][s_non_match,])
		valid$pairs=rbind(dataset$pairs[match_ind[-s_match],],
						  dataset$pairs[-match_ind,][-s_non_match,])

		# split prediction vector, if present
		# c() removes levels, use workaround
		if (!is.null(dataset$prediction))
		{
			train$prediction=c(dataset$prediction[match_ind[s_match]],
						  	   dataset$prediction[-match_ind][s_non_match])
      class(train$prediction) <- "factor"
      levels(train$prediction) <- levels(dataset$prediction)
      
			valid$prediction=c(dataset$prediction[match_ind[-s_match]],
						  	   dataset$prediction[-match_ind][-s_non_match])
      class(valid$prediction) <- "factor"
      levels(valid$prediction) <- levels(dataset$prediction)
		}
							  						
		# split weight vector, if present
		if (!is.null(dataset$Wdata))
		{
			train$Wdata=c(dataset$Wdata[match_ind[s_match]],
						  	   dataset$Wdata[-match_ind][s_non_match])
			valid$Wdata=c(dataset$Wdata[match_ind[-s_match]],
						  	   dataset$Wdata[-match_ind][-s_non_match])
			valid$M=dataset$M
			valid$U=dataset$U
			valid$W=dataset$W
			train$M=dataset$M
			train$U=dataset$U
			train$W=dataset$W
		}

		return (list(train=train,valid=valid))
	}
	
}


getMinimalTrain <- function(rpairs, nEx=1)
{
  # catch erronous input
  if (!(("RecLinkData" %in% class(rpairs)) || 
    "RecLinkResult" %in% class(rpairs)))
    stop("Wrong class for rpairs!")
  
  if (nEx < 1)
    stop(sprintf("Illegal value for nEx: %d!", nEx))
    
  p=rpairs$pairs
  # Zeilen markieren, um Paare identifizieren zu können
  rownames(p)=1:nrow(p)
  p[is.na(p)]=0
  # pro vorhandenem Vergleichsmuster werden bis zu nEx
  # Repräsentanten gezogen
  trainind=unlist(tapply(1:nrow(p),p[,-c(1,2,ncol(p))],
    function(x) if (length(x) > 0) return (x[sample(1:length(x),
      min(length(x),nEx))])
    else return (NULL),
    simplify=FALSE))
  train=rpairs
  train$pairs=rpairs$pairs[trainind,]
  train$Wdata=rpairs$Wdata[trainind]
  train$prediction=rpairs$prediction[trainind]
  return(train)
}

