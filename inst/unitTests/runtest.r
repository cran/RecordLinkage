#!/usr/local/bin/Rscript
library(RUnit)
library(RecordLinkage)
testSuite <- defineTestSuite("Test Suite for Package RecordLinkage",
  ".")
testResult <- runTestSuite(testSuite)
printHTMLProtocol(testResult, fileName="protocol.html")
