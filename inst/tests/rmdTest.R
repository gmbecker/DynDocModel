library(DynDocModel)
options(error=recover)

thing = readRmd(system.file("documents", "minimal.Rmd", package="DynDocModel"))
