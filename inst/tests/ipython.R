library(DynDocModel)
ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

##test tasks
makeTask(ipn[2:3])
collapseTask(ipn[[2]])

makeTask(ipn[2:3])

res = writeIPyNB(ipn)


##test alternatives

makeAltImplSet(ipn[2:4], ipn[5:7])
writeIPyNB(ipn, file="~/gabe/checkedout/ipython/withalts.ipynb")

stuff = readIPyNotebook("~/gabe/checkedout/ipython/withalts.ipynb")
