library(DynDocModel)
ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

evthread = evalDynDoc(ipn)

evthread$children

sapply(evthread$children, function(x) sapply(x$outputs, class))
