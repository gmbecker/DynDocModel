##I completely don't want to write my own "call pandoc" function, but everyone else's versions are hardcoded to assume the input is markdown!!! x.x

pandoc_convert = function(content, in_format = "markdown", out_format = "html", tmpfile = tempfile(), tmpout = makeOutName(tmpfile, out_format), extra_args = "")
{
    
    #pandoc wants UTF-8 all the time!!!
    content = iconv(content, to="UTF-8")
    tmpInName = paste(tmpfile, getPDExt(in_format), sep=".")
    writeTmpFile(content, tmpfile)

    cmd = paste("pandoc", tmpfile, "-f", in_format, "-t", out_format, "-o", tmpout, paste(extra_args,collapse = " "))
    e = tryCatch(system(cmd), error = function(e) e)
    if(is(e, "error"))
        stop("call to pandoc failed. Please make sure pandoc is installed. Exact command was ", cmd)

    out = readTmpFile(tmpout, out_format)
    #pandoc returns UTF-8 all the time!!!
    if(is.character(out))
        iconv(out, from="UTF-8")
    else
        out
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
          saveXML(obj, file = fname, encoding="UTF-8")
      })


setMethod("writeTmpFile", "XMLInteralDocument",
          function(obj, fname)
      {
          saveXML(obj, file = fname, encoding = "UTF-8")
      })

setMethod("writeTmpFile", "character",
          function(obj, fname)
      {
          #for safety. We put them back in once whe know where the content is going (ie not XML/HTML)
          cat(obj, file= fname)
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
    content = paste0("<tmproot>",paste(readLines(file, encoding = "UTF-8"), collapse = "\n"),"</tmproot>")
    content = removeFancyQuotes(content)
    getNodeSet(xmlParse(content, encoding="UTF-8"), "/tmproot/*")

}

readTmpHTML = function(file)
{
    content = paste(readLines(file), collapse = "\n")
    gsub(".*<body>(.*)</body>.*", "\\1", ignore.case = TRUE)
}


rawToDB = function(txt)
{
    txt = removeFancyQuotes(txt)
    blank = grepl("^$", txt)
    
    pars = split(txt, cumsum(blank))
    pars = pars[sapply(pars, function(x) length(x) > 0)]
    tmpdoc = newXMLDoc(node = newXMLNode("tmproot"))
    lapply(pars, function(x) newXMLNode("para", x, parent = xmlRoot(tmpdoc)))
    
}

default_converters = list(default = pandoc_convert, raw = list(docbook = rawToDB))

convertContent = function(content, in_format, out_format, converters = default_converters)
{
    #specific formatter from in_format to out_format
    if(!is.null(converters[[in_format]]) && !is.null(converters[[in_format]][[out_format]]))
        cfun = converters[[in_format]][[out_format]]
    #specialized default for in_format
    else if (!is.null(converters[[in_format]]) && !is.null(converters[[in_format]][["default"]]))
        cfun = converters[[in_format]][["default"]]
    #overall default specified in list
    else if (!is.null(converters[["default"]]))
        cfun = converters[["default"]]
    else
        cfun = pandoc_convert
    
    cfun(content, in_format = in_format, out_format = out_format)
}
