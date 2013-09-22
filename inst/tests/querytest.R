library(DynDocModel)

ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))

makeAltImplSet(ipn[2:4], ipn[5:7])

thr1 = getThread(ipn)

thr2 = getThread(ipn, branch_path="/*/altimpl[2]") #works


nested = readIPyNotebook(system.file("documents/NestedBranching.ipynb", package="DynDocModel"))

instr = expandBranches(nested)

allthr = getAllThreads(nested)
