require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("RecordLinkage")

testSuite <- defineTestSuite("Test Suite for Package RecordLinkage",
  ".")
testResult <- runTestSuite(testSuite)
printHTMLProtocol(testResult, fileName="protocol.html")


