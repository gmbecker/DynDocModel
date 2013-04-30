library(DynDocModel)
ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

res = makeInteractive(ipn[[4]], "x", 10L, "IWidgetSlider", list(range = c(1, 25), step=2), linenums=1L)



writeIPyNB(ipn, file="~/gabe/checkedout/ipython/intcode2.ipynb")

