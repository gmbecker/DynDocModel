library(DynDocModel)
ipn = readDynDoc("~/gabe/checkedout/ipython/withalts.ipynb")

png(filename = "altsvis.png")
res = makeDocumentGraph(ipn)
dev.off()

ipn2 = readDynDoc("~/gabe/checkedout/ipython/withtask.ipynb")
png(filename="taskvis.png")
makeDocumentGraph(ipn2,taskpalette = c("green", "lightgreen", "lightblue", "blue"))
dev.off()

nested = readDynDoc(system.file("documents/NestedBranching.ipynb", package="DynDocModel"))


makeDocumentGraph(nested)

detlevs = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/ClassifyingDigits_Demo.ipynb")
png("~/gabe/checkedout/GabeResearch/Thesis/SoftwareTools/duckgraph.png")
makeDocumentGraph(detlevs)
dev.off()
