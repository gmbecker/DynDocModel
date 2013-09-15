library(DynDocModel)
options(error=recover)

doc =  readRmd(system.file("documents", "minimal.Rmd", package="DynDocModel"))

thr = evalDynDoc(doc)

writeDynDoc(doc = thr, file = "mdtestout.md", output.format = "md")


thr2 = evalDynDoc(getThread(doc, start = doc[[1]], end = doc[[4]]))


writeDynDoc(doc = thr2, file = "~/gabe/checkedout/ipython/ipynbtestout.ipynb", output.format = "ipynb")
