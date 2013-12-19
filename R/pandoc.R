##I completely don't want to write my own "call pandoc" function, but everyone else's versions are hardcoded to assume the input is markdown!!! x.x

pandoc_convert = function(content, in_format = "markdown", out_format = "html", tmpfile = tempfile(), tmpout = makeOutName(tmpfile, out_format), extra_args = "")
{

    tmpInName = paste(tmpfile, getPDExt(in_format), sep=".")
    writeTmpFile(content, tmpfile)

    cmd = paste("pandoc", tmpfile, "-f", in_format, "-t", out_format, "-o", tmpout, paste(extra_args,collapse = " "))
    e = tryCatch(system(cmd), error = function(e) e)
    if(is(e, "error"))
        stop("call to pandoc failed. Please make sure pandoc is installed. Exact command was ", cmd)

    readTmpFile(tmpout, out_format)
}

getPDExt = function(format)
{
    switch(format,
           markdown = "md",
           docbook = "db",
           latex = "tex",
           raw = "txt",
           format)
}

makeOutName = function(infile, outformat)
{
   ext <-getPDExt(outformat)

   paste(infile, ext, sep = ".")
}

setGeneric("writeTmpFile", function(obj, fname) standardGeneric("writeTmpFile"))

setMethod("writeTmpFile", "XMLInteralNode",
          function(obj, fname)
      {
          saveXML(obj, file = fname)
      })


setMethod("writeTmpFile", "XMLInteralDocument",
          function(obj, fname)
      {
          saveXML(obj, file = fname)
      })

setMethod("writeTmpFile", "character",
          function(obj, fname)
      {
          writeLines(obj, con= fname)
      })

readTmpFile = function(file, format)
{
    switch(format,
           docbook = readTmpDB(file),
           html = readTmpHTML(file),
           paste(readLines(file), collapse = "\n")
           )
}

readTmpDB = function(file)
{
    content = paste0("<tmproot>",paste(readLines(file), collapse = "\n"),"</tmproot>")
    getNodeSet(xmlParse(content), "/tmproot/*")

}

readTmpHTML = function(file)
{
    content = paste(readLines(file), collapse = "\n")
    gsub(".*<body>(.*)</body>.*", "\\1", ignore.case = TRUE)
}


rawToDB = function(txt)
{
    blank = grepl("^$", txt)
    
    pars = split(txt, cumsum(blank))
    pars = pars[sapply(pars, function(x) length(x) > 0)]
    tmpdoc = newXMLDoc(node = newXMLNode("tmproot"))
    lapply(pars, function(x) newXMLNode("para", x, parent = xmlRoot(tmpdoc)))
    
}

converters = list(default = pandoc_convert, raw = list(docbook = rawToDB))

convertContent = function(content, in_format, out_format, converters = converters)
{
    if(!is.null(converters[[in_format]]) && !is.null(converters[[in_format]][[out_format]]))
        cfun = converters[[in_format]][[out_format]]
    else if (!is.null(converters[[in_format]]) && !is.null(converters[["default"]]))
        cfun = converters[["default"]]
    else
        cfun = pandoc_convert
    
    cfun(content, in_format = in_format, out_format = out_format)
}
