library(DynDocModel)
options(error=recover)

thing = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/fancyquotetest.ipynb")

writeDynDoc(thing, "fancyquoteTest.Rdb")


doc3 = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/FullHousingEDA.ipynb")
writeDynDoc(doc = doc3, file = "FullHousingEDA.Rdb")

doc4 = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/ClassifyingDigits.ipynb")
writeDynDoc(doc4, "ClassifyingDigits.Rdb")


doc5 = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/ClassifyingDigits_Demo.ipynb")
writeDynDoc(doc5, "ClassifyingDigits_Demo.Rdb")

doc =  readRmd(system.file("documents", "minimal.Rmd", package="DynDocModel"))

thr = evalDynDoc(doc)

writeDynDoc(doc = thr, file = "mdtestout.md", output.format = "md")


thr2 = evalDynDoc(getThread(doc, start = doc[[1]], end = doc[[4]]))


writeDynDoc(doc = thr2, file = "~/gabe/checkedout/ipython/ipynbtestout.ipynb", output.format = "ipynb")


library(RJSONIO)
stuff = "{ \"thing\":{ \"txt\":\"\u201cfancy quotes are lame!\u201d\"}, \"x\":5}"
fromJSON(stuff)

thing = setRefClass("test", fields = list(txt = "character"))

tst = thing$new(txt = "\u201cfancyquotes are lame!\u201d")
