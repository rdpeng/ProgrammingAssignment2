library('RUnit')
test.suite <- defineTestSuite("Main",
                              dirs = file.path("tests"),
                              testFileRegexp = '^.*\\.[rR]$')
test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)
