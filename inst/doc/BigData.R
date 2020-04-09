## ----echo=FALSE,results='hide'----------------------------
backup_options <- options()
options(width=60)

## ----message=FALSE, warnings=FALSE------------------------
library(RecordLinkage)
showClass("RLBigData")
showClass("RLBigDataDedup")
showClass("RLBigDataLinkage")

## ----message=FALSE, warnings=FALSE------------------------
# deduplicate with two blocking iterations and string comparison
data(RLdata500)
data(RLdata10000)
rpairs1 <- RLBigDataDedup(RLdata500, 
           identity = identity.RLdata500, 
           blockfld = list(1,3), strcmp = 1:4)

# link two datasets with phonetic code
s1 <- 471:500
s2 <- sample(1:10000, 300)
identity2 <- c(identity.RLdata500[s1], rep(NaN, length(s2)))
dataset <- rbind(RLdata500[s1,], RLdata10000[s2,])
rpairs2 <- RLBigDataLinkage(RLdata500, dataset, 
           identity1 = identity.RLdata500,
           identity2 = identity2, phonetic = 1:4, 
           exclude = "lname_c2")

## ----message=FALSE, warnings=FALSE------------------------
train <- getMinimalTrain(compare.dedup(RLdata500, 
         identity = identity.RLdata500,
         blockfld = list(1,3)))
rpairs1 <- RLBigDataDedup(RLdata500, 
           identity = identity.RLdata500)
classif <- trainSupv(train, "rpart", minsplit=2)
result <- classifySupv(classif, rpairs1)

## ----message=FALSE, warnings=FALSE------------------------
showClass("RLResult")

## ---------------------------------------------------------
getTable(result)
getErrorMeasures(result)

## ---------------------------------------------------------
rpairs1 <- epiWeights(rpairs1)
result <- epiClassify(rpairs1, 0.5)
getTable(result)

## ---------------------------------------------------------
getPairs(result, min.weight=0.7, filter.link="link")

## ---------------------------------------------------------
getFalsePos(result)
getFalseNeg(result)

## ----echo=FALSE,results='hide'-------------------------------------------
options(backup_options)

