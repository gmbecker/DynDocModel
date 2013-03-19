library(DynDocModel)
ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

makeTask(ipn[2:3])
collapseTask(ipn[[2]])

makeTask(ipn[2:3])

res = writeIPyNB(ipn)
