## cachematrix.R Test Suites

require("RUnit")

source("cachematrix.R")

test.suite <- defineTestSuite("cacheMatrix",
                              dirs = file.path("tests/"),
                              testFileRegexp = 'test.*\\.R')
test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
