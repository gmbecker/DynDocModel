setGeneric("widgetToIPyNBList", function(widget) standardGeneric("widgetToIPyNBList"))

setGeneric("runElement", function(el, evaluators, parent.env, formatters,  ...) standardGeneric("runElement"))

setGeneric("formatOutput", function(object, ...) standardGeneric("formatOutput"))

#setGeneric("getExtension", function(obj) standardGeneric("getExtension"))

#setGeneric("addCellHTML", function(doc, cell, formatters, ...) standardGeneric("addCellHTML"))


  
setGeneric("renderCellIPyNB", function(node, formatters, ...) standardGeneric("renderCellIPyNB"))
setMethod("renderCellIPyNB", c(node="ANY"), function(node, formatters, ...) stop(sprintf("unsupported element class %s", class(node))))
setGeneric("renderCellHTML", function(node, formatters, ...) standardGeneric("renderCellHTML"))
setMethod("renderCellHTML", c(node="ANY"), function(node, formatters, ...) stop(sprintf("unsupported element class %s", class(node))))
setGeneric("renderCellRmd", function(node, formatters, ...) standardGeneric("renderCellRmd"))
setMethod("renderCellRmd", c(node = "ANY"), function(node, formatters, ...) stop(sprintf("unsupported element class %s", class(node))))
setGeneric("renderCellRdb", function(node, formatters, ...) standardGeneric("renderCellRdb"))
setMethod("renderCellRdb", c(node = "ANY"), function(node, formatters, ...) stop(sprintf("unsupported element class %s", class(node))))
setGeneric("renderCellTex", function(node, formatters, ...) standardGeneric("renderCellTex"))
setMethod("renderCellTex", c(node = "ANY"), function(node, formatters, ...) stop(sprintf("unsupported element class %s", class(node))))
setGeneric("renderCellMD", function(node, formatters, ...) standardGeneric("renderCellMD"))
setMethod("renderCellMD", c(node = "ANY"), function(node, formatters, ...) stop(sprintf("unsupported element class %s", class(node))))

setGeneric("getIPyCellType", function(el) standardGeneric("getIPyCellType"))

