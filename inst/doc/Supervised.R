## ----setup, echo=FALSE, message=FALSE, warning=FALSE------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
options(width = 60)
backup_options <- options()

## ----load-library, results='hide', echo=FALSE-------------
library(RecordLinkage)

## ----generate-pairs---------------------------------------
data(RLdata500)
data(RLdata10000)
train_pairs <- compare.dedup(RLdata10000, identity = identity.RLdata10000,
                             n_match = 500, n_non_match = 500)
eval_pairs <- compare.dedup(RLdata500, identity = identity.RLdata500)

## ----training---------------------------------------------
model_rpart <- trainSupv(train_pairs, method = "rpart")
model_bagging <- trainSupv(train_pairs, method = "bagging")
model_svm <- trainSupv(train_pairs, method = "svm")

## ----classification---------------------------------------
result_rpart <- classifySupv(model_rpart, eval_pairs)
result_bagging <- classifySupv(model_bagging, eval_pairs)
result_svm <- classifySupv(model_svm, eval_pairs)

## ----results-rpart, echo=FALSE----------------------------
summary(result_rpart)

## ----results-bagging, echo=FALSE--------------------------
summary(result_bagging)

## ----results-svm, echo=FALSE------------------------------
summary(result_svm)

## ----cleanup, echo=FALSE, results='hide'------------------
options(backup_options)

