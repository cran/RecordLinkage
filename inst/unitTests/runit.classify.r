# RUnit-Tests for (un)supervised classification

# check for errors (illegal arguments etc.)
test.trainSupv.exceptions <- function()
{
  # data setup
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500)
  # errors for argument rpairs:

    # illegal type
    checkException(trainSupv(rpairs$pairs, "rpart"),
      msg="illegal type for rpairs")
    # wrong class
    rpairs2 <- rpairs
    class(rpairs2) <- "list"
    checkException(trainSupv(rpairs2, "bagging"),
      msg="wrong class for rpairs")

    # empty data set
    rpairs2 <- rpairs
    rpairs2$pairs <- rpairs$pairs[0,]
    checkException(trainSupv(rpairs2, "ada"),
      msg="empty data set")

    # only one value for matching status
    rpairs2 <- rpairs
    rpairs2$pairs$is_match=0
    checkException(trainSupv(rpairs2, "svm"),
      msg="only non-matches")
    rpairs2 <- rpairs
    rpairs2$pairs$is_match=1
    checkException(trainSupv(rpairs2, "nnet"),
      msg="only matches")
    rpairs2 <- rpairs
    rpairs2$pairs$is_match=NA
    checkException(trainSupv(rpairs2, "rpart"),
    "only unknown pairs")
    
    # only one training example
    rpairs2 <- rpairs
    rpairs2$pairs <- rpairs$pairs[1,]
    checkException(trainSupv(rpairs2, "ada"),
    "only one training example")
    
  
  # errors for argument method
    # illegal type
    checkException(trainSupv(rpairs, method=12),
      msg="illegal type for method")
    checkException(trainSupv(rpairs, method=TRUE),
      msg="illegal type for method")

    # unknown value
    checkException(trainSupv(rpairs, method="nonexisting"),
      msg="illegal method")

    
  # errors for argument use.pred
    # none, ignored if not TRUE
  
  # errors for argument omit.possible
    # none, ignored if not TRUE

  # errors for argument convert.na
    # none, ignored if not TRUE

  # errors for argument omit.possible
    # none, ignored if not TRUE

  # errors for combination of rpairs and method
    # none
    
  # errors for combination of rpairs and use.pred
    # no prediction vector present
    checkException(trainSupv(rpairs, method="bagging", use.pred=TRUE),
      msg="use pred but no prediction vector found")
    # only one prediction
    rpairs2 <- rpairs
    rpairs2$prediction <- rep(0,nrow(rpairs2$pairs))
    checkException(trainSupv(rpairs2, "svm", use.pred=TRUE),
      msg="only non-link predictions") 
    rpairs2 <- rpairs
    rpairs2$prediction <- rep(1,nrow(rpairs2$pairs))
    checkException(trainSupv(rpairs2, "svm", use.pred=TRUE),
      msg="only non-link predictions") 
    rpairs2 <- rpairs
    rpairs2$prediction <- rep(NA,nrow(rpairs2$pairs))
    checkException(trainSupv(rpairs2, "svm", use.pred=TRUE),
      msg="only possible predictions") 
      
  # errors for combination of rpairs and omit.possible
    # deleting possible values leaves only one example
    rpairs2 <- rpairs
    rpairs2$pairs$is_match <- c(1, rep(NA,nrow(rpairs2$pairs)-1))
    checkException(trainSupv(rpairs2, method="ada", omit.possible=TRUE),
      msg="deleting unknown pairs leaves only one example")
  
  # errors for combination of rpairs and convert.na
    # conversion leaves only one distinct example
    rpairs2 <- rpairs
    rpairs2$pairs <- as.data.frame(matrix(
      c(1,1,1,0,1,0,1,0,1,0,
        1,2,1,NA,1,NA,1,0,1,1), byrow=TRUE, 
        dimnames=list(NULL,colnames(rpairs$pairs)), nrow=2, ncol=10))
  checkException(trainSupv(rpairs2, "nnet", convert.NA=TRUE),
    msg="conversion of NAs leaves only one distinct example")
  
  # errors for combination of rpairs and include.data
    # none
  
  # errors for combination of three or more arguments
    # rpairs, omit.possible and use.pred leads to only one example     
    rpairs2 <- rpairs
    rpairs2$prediction <- factor(c("N", rep("P",nrow(rpairs2$pairs)-1)),
      levels=c("N","P","L"))
    checkException(trainSupv(rpairs2, method="bagging", omit.possible=TRUE,
    use.pred=TRUE),
      msg="deleting possibles from prediction leaves only one example")
    # same combination leads to only matches
    rpairs2 <- rpairs
    rpairs2$prediction <- factor(c("L", "L", rep("P",nrow(rpairs2$pairs)-2)),
      levels=c("N","P","L"))
    checkException(trainSupv(rpairs2, method="rpart", omit.possible=TRUE,
    use.pred=TRUE),
      msg="deleting possibles from prediction leaves only matches")
    # same combination leads to only non-matches
    rpairs2 <- rpairs
    rpairs2$prediction <- factor(c("N", "N", rep("P",nrow(rpairs2$pairs)-2)),
      levels=c("N","P","L"))
    checkException(trainSupv(rpairs2, method="bagging", omit.possible=TRUE,
    use.pred=TRUE),
      msg="deleting possibles from prediction leaves only non-matches")

    # rpairs, omit.possible and convert.na leads to only one distinct example    
    rpairs2 <- rpairs
    rpairs2$pairs <- as.data.frame(matrix(
      c(1,1,1,0,1,0,1,0,1,0,
        1,2,0,NA,1,0,1,1,1,NA,
        1,3,1,NA,1,NA,1,0,1,1), byrow=TRUE,
        dimnames=list(NULL,colnames(rpairs$pairs)), nrow=3, ncol=10))
  checkException(trainSupv(rpairs2, "nnet", convert.NA=TRUE, omit.possible=TRUE),
    msg="conversion of NAs and omitting possibles leaves only one distinct example")

    # rpairs, omit.possible, convert.na and use.pred leads to only one distinct example
    rpairs2 <- rpairs
    rpairs2$pairs <- as.data.frame(matrix(
      c(1,1,1,0,1,0,1,0,1,NA,
        1,2,0,NA,1,0,1,1,1,NA,
        1,3,1,NA,1,NA,1,0,1,NA), byrow=TRUE,
         dimnames=list(NULL,colnames(rpairs$pairs)), nrow=3, ncol=10))
    rpairs2$prediction <- factor(c("N", "P", "L"))
  checkException(trainSupv(rpairs2, "nnet", convert.NA=TRUE, omit.possible=TRUE,
    use.pred=TRUE),
    msg="conversion of NAs and omitting possibles leaves only one distinct example")
    
}

test.trainSupv <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500,
    blockfld=list(1,3,5,6,7))
  
  # check if generated models have the right properties
    classif <- trainSupv(rpairs, method="rpart")
    checkEquals(class(classif$model), "rpart",
      msg=sprintf("check class of generated model"))
    checkEquals(classif$method, "rpart",
      msg=sprintf("check method component"))
    # check class only once
    checkEquals(class(classif), "RecLinkClassif")

    classif <- trainSupv(rpairs, method="bagging")
    checkEquals(class(classif$model), "classbagg",
      msg=sprintf("check class of generated model"))
    checkEquals(classif$method, "bagging",
      msg=sprintf("check method component"))

    classif <- trainSupv(rpairs, method="ada")
    checkEquals(class(classif$model), "ada",
      msg=sprintf("check class of generated model"))
    checkEquals(classif$method, "ada",
      msg=sprintf("check method component"))

    classif <- trainSupv(rpairs, method="svm")
    checkEquals(class(classif$model), c("svm.formula","svm"),
      msg=sprintf("check class of generated model"))
    checkEquals(classif$method,"svm" ,
      msg=sprintf("check method component"))

    classif <- trainSupv(rpairs, method="nnet")
    checkEquals(class(classif$model), c("nnet.formula", "nnet"),
      msg=sprintf("check class of generated model"))
    checkEquals(classif$method, "nnet",
      msg=sprintf("check method component"))
      
  # check that all attributes are included in the model
  classif <- trainSupv(rpairs, "svm")
  checkEquals(attr(classif$model$terms, "term.labels"),
    colnames(rpairs$pairs[,-c(1,2,ncol(rpairs$pairs))]),
    msg="Check attributes in model")

  # use result object with prediction vector
  result <- classifyUnsup(rpairs, "kmeans")
  result$pairs$is_match=NA
  classif <- trainSupv(result, "svm", use.pred=TRUE)
  checkEquals(class(classif), "RecLinkClassif",
    msg="Use result object for calibration")

  # check if copied data is identical with include.data = TRUE
  classif <- trainSupv(rpairs, "rpart", include.data=TRUE, convert.na=FALSE)
  checkEquals(classif$train, rpairs,
    msg="Check copied data")

  # check copied data when using convert.na = TRUE
  classif <- trainSupv(rpairs, "rpart", include.data=TRUE, convert.na=TRUE)
  checkEquals(classif$train, rpairs,
    msg="Check copied data")

  # check copied data when using omit.possible = TRUE
  rpairs2 <- rpairs
  rpairs2$pairs[runif(1)*nrow(rpairs2$pairs),"is_match"]=NA
  classif <- trainSupv(rpairs2, "svm", include.data=TRUE, omit.possible=TRUE)
  checkEquals(classif$train, rpairs2,
      msg="Check copied data")

  # check copied data when using use.pred = TRUE
  rpairs2 <- rpairs
  rpairs2$prediction <- factor(rep("P", nrow(rpairs2$pairs)), levels=c("N","P","L"))
  rpairs2$prediction[rpairs2$pairs$is_match==1]="L"
  rpairs2$prediction[rpairs2$pairs$is_match==0]="N"  
  classif <- trainSupv(rpairs2, "svm", include.data=TRUE, omit.possible=TRUE)
  checkEquals(classif$train, rpairs2,
    msg="Check copied data")
  
  # check if training with use.pred yields the same result
  set.seed(1)
  result1 <- trainSupv(rpairs, "rpart", xval=0)
  rpairs2 <- rpairs
  rpairs2$prediction <- factor(rep("P", nrow(rpairs2$pairs)), levels=c("N","P","L"))
  rpairs2$prediction[rpairs2$pairs$is_match==1]="L"
  rpairs2$prediction[rpairs2$pairs$is_match==0]="N"  
  set.seed(1)
  result2 <- trainSupv(rpairs2, "rpart", use.pred = TRUE, xval=0)
  checkEquals(result1, result2, 
    msg="Check equal result when using prediction vector")  

  # check if examples are omitted correctly
  rpairs2 <- rpairs
  # choose some examples to be set to unknown match status
  s <- sample(1:nrow(rpairs2$pairs), sample(1:(nrow(rpairs2$pairs)/2),1))
  is.na(rpairs2$pairs$is_match) <- s
  classif <- trainSupv(rpairs2, "rpart", omit.possible=TRUE, convert.na=FALSE, x=TRUE)
  checkEqualsNumeric(as.matrix(classif$model$x), as.matrix(rpairs2$pairs[-s,-c(1,2,ncol(rpairs$pairs))]),
    msg="Check if unknown pairs are omitted correctly")
  # check match status after conversion to factors (0 -> 1 "N", 1 -> 3 "L")
  checkEqualsNumeric(as.matrix(classif$model$y), as.double(rpairs2$pairs$is_match[-s]) * 2 + 1,
    msg="Check if unknown pairs are omitted correctly")

  # check if examples are omitted correctly with using prediction
  rpairs2 <- rpairs
  s <- sample(1:nrow(rpairs2$pairs), sample(1:(nrow(rpairs2$pairs)/2),1))
  is.na(rpairs2$pairs$is_match) <- s
  rpairs2$prediction <- factor(rep("P", nrow(rpairs2$pairs)), levels=c("N","P","L"))
  rpairs2$prediction[rpairs2$pairs$is_match==1]="L"
  rpairs2$prediction[rpairs2$pairs$is_match==0]="N"  
  rpairs2$pairs$is_match=NA
  classif <- trainSupv(rpairs2, "rpart", omit.possible=TRUE, convert.na=FALSE, 
    x=TRUE, use.pred=TRUE)
  checkEqualsNumeric(as.matrix(classif$model$x), as.matrix(rpairs2$pairs[-s,-c(1,2,ncol(rpairs$pairs))]),
    msg="Check if unknown pairs are omitted correctly using prediction")
  # check match status after conversion to factors (0 -> 1 "N", 1 -> 3 "L")
  checkEqualsNumeric(as.matrix(classif$model$y), as.double(rpairs2$prediction[-s]),
    msg="Check if unknown pairs are omitted correctly using prediction")


  # check if NAs are converted correctly
  rpairs2 <- rpairs
  classif <- trainSupv(rpairs2, "rpart", convert.na=TRUE, x=TRUE)
  rpairs2$pairs[is.na(rpairs2$pairs)]=0
  checkEqualsNumeric(as.matrix(classif$model$x), as.matrix(rpairs2$pairs[,-c(1,2,ncol(rpairs$pairs))]),
    msg="Check if unknown pairs are omitted correctly")
  # check match status after conversion to factors (0 -> 1 "N", 1 -> 3 "L")
  checkEqualsNumeric(as.matrix(classif$model$y), as.double(rpairs2$pairs$is_match) * 2 + 1,
    msg="Check if unknown pairs are omitted correctly")


  # use factor for method, should be converted to character and work properly
  set.seed(1)
  result1 <- trainSupv(rpairs, "rpart", xval=0)
  set.seed(1)
  result2 <- trainSupv(rpairs, factor("rpart"), xval=0)
  checkEquals(result1, result2)
}




# check for errors (illegal arguments etc.)
test.classifyUnsup.exceptions <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500)

  # errors concerning argument rpairs
    # illegal type
    # illegal type
    checkException(classifyUnsup(rpairs$pairs, "kmeans"),
      msg="illegal type for rpairs")

    # wrong class
    rpairs2=rpairs
    class(rpairs2)="list"
    checkException(classifyUnsup(rpairs$pairs, "kmeans"),
      msg="illegal class for rpairs")
    # empty data set
    rpairs2$pairs=data.frame()
    checkException(classifyUnsup(rpairs$pairs, "kmeans"),
      msg="empty data set")

  # errors for argument method
    # illegal type
    checkException(classifyUnsup(rpairs, method=2),
      msg="illegal type for method")
    checkException(classifyUnsup(rpairs, method=TRUE),
      msg="illegal type for method")

    # unknown value
    checkException(classifyUnsup(rpairs, method="nonexisting"),
      msg="illegal method")

}


test.classifyUnsup <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500)
  
  result <- classifyUnsup(rpairs,"kmeans")
  # check class of result
  checkEquals(class(result)[1], "RecLinkResult", 
  msg = " check class of result")
  # check copied components
  checkEquals(result$pairs, rpairs$pairs, 
    msg = "check copied pairs component")
  checkEquals(result$data, rpairs$data, 
    msg = "check copied data component")
  checkEquals(result$frequencies, rpairs$frequencies, 
    msg = "check copied frequencies component")

  # check prediction
  checkEqualsNumeric(length(result$prediction), nrow(rpairs$pairs), 
    msg = "check length of prediction")
  checkEquals(class(result$prediction), "factor",
  msg = "check class of prediction")
  checkEquals(levels(result$prediction), c("N", "P", "L"),
    msg = "check levels of prediction")
}

test.classifySupv.exceptions <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500,
    blockfld=list(5:6, 6:7, c(5,7)))
  data(RLdata10000)
  newdata <- compare.dedup(RLdata10000, identity=identity.RLdata10000,
    blockfld=list(5:6, 6:7, c(5,7)))
  model <- trainSupv(rpairs, "svm")

  # errors concerning model
    # wrong class
    model2 <- model
    class(model2) <- "wrong"
    checkException(classifySupv(model2, newdata),
      msg = "wrong class for model")
    # wrong type
    checkException(classifySupv(model$model2, newdata),
      msg = "wrong type for model")
    # unknown type / model
    model2 <- model
    model2$method <- "nonexisting"
    checkException(classifySupv(model2, newdata,
      msg = "wrong method"))

    model2 <- model
    class(model2$model) <- "glm"
    checkException(classifySupv(model2, newdata,
      msg = "wrong model"))

  # errors concerning newdata
    # wrong class
    newdata2 <- newdata
    class(newdata2) <- "RecLinkError"
    checkException(classifySupv(model, newdata2,
      msg = "wrong class of newdata"))
    # wrong type
    checkException(classifySupv(model, newdata$pairs,
      msg = "wrong type of newdata"))
    # empty data set
    newdata2 <- newdata
    newdata2$pairs <- newdata$pairs[0,]
    checkException(classifySupv(model, newdata2$pairs,
      msg = "no record pairs to classify"))
  # errors concerning combination of model and newdata
    # different format of training and new data
    newdata2 <- newdata
    newdata2$pairs[,2] <- NULL
    checkException(classifySupv(model, newdata2,
      msg = "format of newdata does not match model"))
}

test.classifySupv <- function()
{
  data(RLdata500)
  rpairs <- compare.dedup(RLdata500, identity=identity.RLdata500,
    blockfld=list(5:6, 6:7, c(5,7)))
  data(RLdata10000)
  newdata <- compare.dedup(RLdata10000, identity=identity.RLdata10000,
    blockfld=list(5:6, 6:7, c(5,7)))

  for (method in c("rpart", "bagging", "ada", "svm", "nnet"))
  {
    classif <- trainSupv(rpairs, method=method)
    result <- classifySupv(classif, newdata)
    # check class
    checkEquals(class(result)[1], "RecLinkResult",
      msg = sprintf(" check class of result for method %s", method))
    checkEquals(class(result)[1], "RecLinkResult",
      msg = sprintf(" check class of result for method %s", method))

    # check copied components
    checkEquals(result$pairs, newdata$pairs, 
      msg = sprintf(" check pairs component of result for method %s", method))
    checkEquals(result$data, newdata$data, 
      msg = sprintf(" check data component of result for method %s", method))
    checkEquals(result$frequencies, newdata$frequencies, 
      msg = sprintf(" check frequencies component of result for method %s", method))

    # check prediction
    checkEqualsNumeric(length(result$prediction), nrow(newdata$pairs), 
      msg = sprintf(" check length of prediction for method %s", method))
    checkEquals(class(result$prediction), "factor",
      msg = sprintf(" check class of prediction for method %s", method))
    checkEquals(levels(result$prediction), c("N", "P", "L"),
      msg = sprintf(" check levels of prediction for method %s", method))

    # feasible results are hard to check, check that more non-matches than 
    # matches exist
    checkTrue(sum(result$prediction=="N") > sum(result$prediction=="L"),
      msg = sprintf(" check feasible match proportion for method %s", method))
  }
}
