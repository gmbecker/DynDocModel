if(FALSE)
    {
setMethod("getExtension", "HTMLFormat", function(obj) "html")
setMethod("getExtension", "PDFFormat", function(obj) "pdf")
setMethod("getExtension", "RdbFormat", function(obj) "Rdb")
setMethod("getExtension", "RmdFormat", function(obj) "Rmd")
setMethod("getExtension", "MDFormat", function(obj) "md")
setMethod("getExtension", "IPyNBFormat", function(obj) "ipynb")
setMethod("getExtension", "ANY", function(obj) stop("unknown extension"))
}
