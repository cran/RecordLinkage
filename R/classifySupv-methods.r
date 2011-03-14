# Methods for s$ generic classifyUnsupv


setGeneric(
  name= "classifySupv",
  def=function(model, newdata, ...){standardGeneric("classifySupv")}
)

#' Method for old (S3-style) objects, replaces ordinary function
setMethod(
  f = "classifySupv",
  signature = c("RecLinkClassif", "RecLinkData"),
  definition = function (model, newdata, ...)
  {

    # type checks from previous version omitted, now enforced by 
    # method dispatching  
        
  	ret=newdata
  
    x=newdata$pairs[,-c(1,2,ncol(newdata$pairs))]
    if(any(colnames(x)!=model$attrNames))
    {
      warning("Attribute names in newdata differ from training set!")
      colnames(x)=model$attrNames
    }
  	x[is.na(x)]=0
  
      predict=switch(model$method,
    		svm=predict(model$model, newdata=x,...),       
  	 	 rpart=predict(model$model, newdata=x,type="class",...),       
  		  ada=predict(model$model, newdata=x,type="vector",...),       
  		  bagging=predict(model$model, newdata=x,type="class",...),
  		  nnet=predict(model$model, newdata=x,type="class",...),
        stop("Illegal classification method!"))
      # refactor to ensure uniform order of levels
      ret$prediction=factor(predict,levels=c("N","P","L"))
      class(ret)="RecLinkResult"
      return(ret)
  }
)

# Methods for big data sets
setMethod(
  f = "classifySupv",
  signature = c("RecLinkClassif", "RLBigData"),
  definition = function(model, newdata, withProgressBar = (sink.number()==0), ...)
  {
    links <- matrix(0L, 0L, nrow=0, ncol=2)
    possibleLinks <- matrix(0L, 0L, nrow=0, ncol=2)
    nPairs <- 0
    if (withProgressBar)
    {
      expPairs <- getExpectedSize(newdata)
      pgb <- txtProgressBar(max=expPairs)
    }

    on.exit(clear(newdata))
    newdata <- begin(newdata)

    while(nrow(slice <- nextPairs(newdata)) > 0)
    {
      # Spaltennamen angleichen -> funktioniert so nicht!
      # TODO: Fehlerbehandlung für ungleiche Attributanzahl
      #     colnames(slice) <- c("id1", "id2", levels(model$model$frame$var)[-1])
      slice[is.na(slice)] <- 0
      prediction=switch(model$method,
        svm=predict(model$model, newdata=slice,...),
          rpart=predict(model$model, newdata=slice,type="class",...),
      	  ada=predict(model$model, newdata=slice,type="vector",...),
      	  bagging=predict(model$model, newdata=slice,type="class",...),
      	  nnet=predict(model$model, newdata=slice,type="class",...),
          stop("Illegal classification method!"))
      links <- rbind(links, as.matrix(slice[prediction=="L",1:2]))
      possibleLinks <- rbind(possibleLinks, as.matrix(slice[prediction=="P",1:2]))
      nPairs <- nPairs + nrow(slice)

      if (withProgressBar)
      {
        setTxtProgressBar(pgb, nPairs)
        flush.console()
      }
    }

    if (withProgressBar) close(pgb)

    result <- new("RLResult", data = newdata, links = links,
      possibleLinks = possibleLinks, nPairs = nPairs)
  }
)
