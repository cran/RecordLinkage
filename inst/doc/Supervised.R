## ----echo=FALSE,results='hide'----------------------------
knitr::opts_chunk$set(message =FALSE, warnings = FALSE)
options(width=60)
backup_options <- options()

## ----include=FALSE----------------------------------------
library(knitr)
opts_chunk$set(
concordance=TRUE
)

## ----results='hide',echo=FALSE----------------------------
library(RecordLinkage)

## ---------------------------------------------------------
data(RLdata500)
data(RLdata10000)
train_pairs=compare.dedup(RLdata10000, identity=identity.RLdata10000,
  n_match=500, n_non_match=500)

eval_pairs=compare.dedup(RLdata500,identity=identity.RLdata500)

## ---------------------------------------------------------
model_rpart=trainSupv(train_pairs, method="rpart")
model_bagging=trainSupv(train_pairs, method="bagging")
model_svm=trainSupv(train_pairs, method="svm")

## ---------------------------------------------------------
result_rpart=classifySupv(model_rpart, eval_pairs)
result_bagging=classifySupv(model_bagging, eval_pairs)
result_svm=classifySupv(model_svm, eval_pairs)

## ----results='asis',echo=FALSE----------------------------
texSummary(result_rpart)

## ----results='asis',echo=FALSE----------------------------
texSummary(result_bagging)

## ----results='asis',echo=FALSE----------------------------
texSummary(result_svm)

## ----echo=FALSE,results='hide'----------------------------
options(backup_options)

