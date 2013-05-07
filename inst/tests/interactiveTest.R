library(DynDocModel)
ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

res = makeInteractive(ipn[[4]], "x", 10L, "IWidgetSlider", list(range = c(1, 25), step=2), linenums=1L)



writeIPyNB(ipn, file="~/gabe/checkedout/ipython/intcode2.ipynb")

ksm = readIPyNotebook("~/gabe/checkedout/ipython/InteractiveKSmooth.ipynb")

makeInteractive(ksm[[4]], "bw", 15L, "IWidgetSlider", list(range = c(5, 50), step = 5), linenums = 1L)
writeIPyNB(ksm, file="~/gabe/checkedout/ipython/InteractiveKSmooth.ipynb")


stuff = readIPyNotebook("~/gabe/checkedout/ipython/InteractiveKSmooth.ipynb")
