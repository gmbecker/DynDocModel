library(DynDocModel)
ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

evthread = evaluate(ipn)
