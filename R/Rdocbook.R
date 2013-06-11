readRdb = function(doc, xsl = NULL, ...)
  {
    if(is(doc, "character"))
      doc = xmlParse(doc)
    
    textcollection = list()
    .doTextNode = function(parent)
      {
        txt = dbElement$new(content = textcollection)
        parent$addChild(txt)
        textcollection <<- list()
      }
    retdoc = new("DynDoc")
    xpathApply(doc, "/*", processRdbNode, parent = retdoc, xsl = xsl)
    
    retdoc
  }

processRdbNode = function(node, parent, ...)
  {
    switch(xmlName(node),
           sect1 = processRdbSect(node, parent, level = 1, xsl = xsl, ...),
           sect2 = processRdbSect(node, parent, level = 2, xsl = xsl, ...),
           sect3 = processRdbSect(node, parent, level =3, xsl = xsl, ...),
           task = processRdbTask(node, parent, xsl = xsl, ...),
           para = processRdbPara(node, parent, xsl = xsl, ...),
           code = processRdbCode(node, parent, xsl = xsl, ...),
            stop(paste0("node type", xmlName(node), "not yet supported"))
            )
   }

processRdbSect = function(node, parent, level, ...)
  {
    sect = sectElement$new(formatSpecific = list(level = level))
    if("title" %in% names(node))
      sect$title = xmlValue(node[["title"]])

    xpathApply(node,"/*[not(self::title)]", fun = processRdbNode, parent = sect)
    if(length(textcollection))
     .doTextNode(parent = sect)
  }

processRdbPara = function(node, parent, xsl, ...)
  {
    #XXX for now I will assume that r:code is directly under para. This will need to be relaxed in the future...
    hasRcode = length(getNodeSet(node, "//x:code", namespaces=c(x="http://www.r-project.org"))) > 0

    if(!hasRcode)
      textcollection <<- c(textcollection, node)
    else
      {
        #This is seriously SUPER F-ing HACKY!!!! Bad Gabe!!!!
        #Should be using a SAX parser or something else to deal with this whole mess!
        tmp = saveXML(node)
        tmp = gsub("<r:code>", "</para><r:code>", tmp)
        tmp = gsub("</r:code>", "<r:code><para>", tmp)
        tmp = paste0("<root>\n", tmp, "\n</root>")
        tmpnode = xmlRoot(xmlParse(tmp))
        xpathApply(tmpnode, "/*", function(nd)
                   {
                     if(xmlName(nd) == "para")
                       textcollection <<- c(textcollection, nd)
                     else if (grepl("code", xmlName(nd)))
                       {
                         .doTextNode(parent = parent)
                         processRdbCode(nd, xsl = xsl, in.last.para = TRUE)
                       }
                   })
      }
  }

processRdbCode = function(node, parent, xsl, in.last.para = FALSE)
  {
    code = xmlValue(node)

    codel = rCodeElement$new(content = code, formatSpecific = list(in.last.para = in.last.para))

    parent$addChild(codel)


  }
