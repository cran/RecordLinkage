## ----echo=FALSE,results='hide'----------------------------
knitr::opts_chunk$set(message =FALSE, warnings = FALSE)
options(width=60)
backup_options <- options()

## ----echo=FALSE, results='hide'---------------------------
library(RecordLinkage)

## ---------------------------------------------------------
data(RLdata500)
RLdata500[1:5,]

## ---------------------------------------------------------
pairs=compare.dedup(RLdata500,identity=identity.RLdata500,
      blockfld=list(c(5,6),c(6,7),c(5,7)))
summary(pairs)

## ---------------------------------------------------------
pairs=emWeights(pairs)
hist(pairs$Wdata, plot=FALSE)

## ----results='hide'---------------------------------------
getPairs(pairs,30,20)

## ----echo=FALSE-------------------------------------------
getPairs(pairs,30,20)[23:36,]

## ---------------------------------------------------------
pairs=emClassify(pairs, threshold.upper=24, threshold.lower=-7)
summary(pairs)

## ---------------------------------------------------------
possibles <- getPairs(pairs, show="possible")
possibles[1:6,]
links=getPairs(pairs,show="links", single.rows=TRUE)
link_ids <- links[, c("id1", "id2")]
link_ids

## ----echo=FALSE,results='hide'----------------------------
options(backup_options)

