## ----setup, echo=FALSE, message=FALSE, warning=FALSE------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
options(width = 60)
backup_options <- options()

## ----load-library, echo=FALSE, results='hide'-------------
library(RecordLinkage)

## ----load-data--------------------------------------------
data(RLdata500)
RLdata500[1:5,]

## ----compare-dedup----------------------------------------
pairs <- compare.dedup(RLdata500, identity = identity.RLdata500,
                       blockfld = list(c(5,6), c(6,7), c(5,7)))
summary(pairs)

## ----em-weights-------------------------------------------
pairs <- emWeights(pairs)
hist(pairs$Wdata, plot = FALSE)

## ----get-pairs-hidden, results='hide'---------------------
getPairs(pairs, 30, 20)

## ----get-pairs-shown, echo=FALSE--------------------------
getPairs(pairs, 30, 20)[23:36,]

## ----em-classify------------------------------------------
pairs <- emClassify(pairs, threshold.upper = 24, threshold.lower = -7)
summary(pairs)

## ----final-pairs------------------------------------------
possibles <- getPairs(pairs, show = "possible")
possibles[1:6,]
links <- getPairs(pairs, show = "links", single.rows = TRUE)
link_ids <- links[, c("id1", "id2")]
link_ids

## ----cleanup, echo=FALSE, results='hide'------------------
options(backup_options)

