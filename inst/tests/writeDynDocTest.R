library(DynDocModel)
options(error=recover)



doc3 = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/FullHousingEDA.ipynb")
writeDynDoc(doc = doc3, file = "FullHousingEDA.Rdb")

doc4 = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/ClassifyingDigits.ipynb")
writeDynDoc(doc4, "ClassifyingDigits.Rdb")

doc =  readRmd(system.file("documents", "minimal.Rmd", package="DynDocModel"))

thr = evalDynDoc(doc)

writeDynDoc(doc = thr, file = "mdtestout.md", output.format = "md")


thr2 = evalDynDoc(getThread(doc, start = doc[[1]], end = doc[[4]]))


writeDynDoc(doc = thr2, file = "~/gabe/checkedout/ipython/ipynbtestout.ipynb", output.format = "ipynb")

"<tmproot><para>\n  We can see from this that all the \035action\035 is happening in the center\n  of the images. In particular, many\n</para>\n<para>\n  of the pixels far from the center have virtually no variance (because\n  they are always 0). This suggests\n</para>\n<para>\n  that this dataset is likely to benefit greatly from dimension\n  reduction methods, as those pixels are not\n</para>\n<para>\n  useful in the first place.\n</para>\n<para>\n  We now turn to principle components as our dimension reduction method\n  of choice. We can implement PCA by hand or use one of a number of\n  different existing functions.\n</para></tmproot>"
