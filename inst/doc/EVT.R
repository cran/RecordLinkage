## ----echo=FALSE,results='hide'----------------------------
knitr::opts_chunk$set(message =FALSE, warnings = FALSE)
options(width=60)
backup_options <- options()

## ----results='hide',echo=FALSE----------------------------
library(RecordLinkage)

## ---------------------------------------------------------
data(RLdata500)
bf=list(1,3,5,6,7)
rpairs=compare.dedup(RLdata500,identity=identity.RLdata500,
  blockfld=bf,strcmp=1:4)
  rpairs=emWeights(rpairs)

## ----results='hide'---------------------------------------
## Not run: getParetoThreshold(rpairs)

## ----echo=FALSE-------------------------------------------
plotMRL(rpairs)

## ----echo=FALSE-------------------------------------------
plotMRL(rpairs)
abline(v=c(1.2,12.8),col="red",lty="dashed")
l=mrl(rpairs$Wdata)
range=l$x>1.2 & l$x < 12.8
points(l$x[range], l$y[range],col="red",type="l")

## ---------------------------------------------------------
threshold=getParetoThreshold(rpairs,interval=c(1.2,12.8))
result=emClassify(rpairs,threshold)
summary(result)

## ----echo=FALSE,results='hide'----------------------------
options(backup_options)

