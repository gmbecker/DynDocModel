library(DynDocModel)
ipn = readDynDoc("~/gabe/checkedout/ipython/withalts.ipynb")

png(filename = "altsvis.png")
res = makeDocumentGraph(ipn)
dev.off()

ipn2 = readDynDoc("~/gabe/checkedout/ipython/withtask.ipynb")
png(filename="taskvis.png")
makeDocumentGraph(ipn2,taskpalette = c("green", "lightgreen", "lightblue", "blue"))
dev.off()

