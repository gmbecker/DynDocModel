library(RUnit)
library(DynDocModel)

testSuite = defineTestSuite(name = "DynDocModelTests",
  dirs = getwd(),
  testFileRegexp="^test.*\\.R$",
  testFuncRegexp="^test.*")

tests = runTestSuite(testSuite)
printTextProtocol(tests, showDetails=FALSE)
