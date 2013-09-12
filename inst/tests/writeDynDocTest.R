library(DynDocModel)
options(error=recover)

doc =  readRmd(system.file("documents", "minimal.Rmd", package="DynDocModel"))

thr = evalDynDoc(doc)

writeDynDoc(doc = thr, file = "mdtestout.md", output.format = "md")
