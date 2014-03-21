library(DynDocModel)
library(markdown)
doc = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/ClassifyingDigits_Demo.ipynb")
oldwd = getwd()
setwd("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks")

thr = evalDynDoc(getThread(doc))
setwd(oldwd)

tangle_code = function(node, ...) node$content 

tangle_text = function(node, ...)  character() 

tangle_cont = function(node,  ...) {
  content = lapply(node$children, renderElement, ...)
  unlist(content, recursive=TRUE)
}

tangle_inst = function(node, ...) {
  if(length(node$children))
    tangle_cont(node, ...)
  else 
    renderElement(node$element, ...)
}

rends = list("RCodeElement" = tangle_code,
             "ContainerElement" = tangle_cont,
             "ElementInstance" = tangle_inst,
             "default" = function(...) character())

writeDynDoc(thr, "tangled.R", formatters = list(), cell.renderers = rends)
