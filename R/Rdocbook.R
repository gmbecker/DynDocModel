.doTextNode = function(parent, state)
{
    if(length(state$textcollection))
    {
        txt = dbElement$new(content = state$textcollection)
        parent$addChild(txt)
        state$textcollection <- list()
    }
}

readRdb = function(doc, xsl = NULL, ...)
{
    if(is(doc, "character"))
        doc = xmlParse(doc)
    
    state = new.env()
    state$textcollection = list()
    retdoc = new("DynDoc")
    xpathApply(xmlRoot(doc), "./*", processRdbNode, parent = retdoc, xsl = xsl, state = state)
    .doTextNode(retdoc, state)
    retdoc
}

processRdbNode = function(node, parent, xsl = NULL, state, ...)
{
    switch(xmlName(node),
           sect1 = processRdbSect(node, parent, level = 1, xsl = xsl, state, ...),
           section = processRdbSect(node, parent, level = 1, xsl = xsl, state, ...), 
           sect2 = processRdbSect(node, parent, level = 2, xsl = xsl, state,  ...),
           sect3 = processRdbSect(node, parent, level =3, xsl = xsl, state,  ...),
           task = processRdbTask(node, parent, xsl = xsl, state, ...),
#           branchset = processRdbBranchSet(node, parent, xsl, state, ...),
#           branching = processRdbBranchSet(node, parent, xsl, state, ...),
#           altImplementations = processRdbBranchSet(node, parent, xsl, state, ...),
#           altMethods = processRdbBranchSet(node, parent, xsl, state, ...),
#           branch = processRdbBranch(node, parent, xsl, state, ...),
#           altImplementation = processRdbBranch(node, parent, xsl, state, ...),
#           altMethod = processRdbBranch(node, parent, xsl, state, ...),
#           branch = processRdbBranch(node, parent, xsl, state, ...),
           para = processRdbPara(node, parent, xsl = xsl, state, ...),
           code = processRdbCode(node, parent, xsl = xsl, state, ...),

           )
}


processRdbTask = function(node, parent, xsl = NULL, state, ...)
{
    task = taskElement$new()
    
    xpathApply(node,"./*", fun = processRdbNode, parent = task)
    if(length(state$textcollection))
        .doTextNode(parent = task, state = state)
}



processRdbSect = function(node, parent, xsl = NULL, level, state, ...)
{
    .doTextNode(parent, state)
    sect = sectElement$new(formatSpecific = list(level = level))
    if("title" %in% names(node))
        sect$title = xmlValue(node[["title"]])
    
    xpathApply(node,"./*[not(name() = 'title')]", fun = processRdbNode, parent = sect, xsl = xsl, state = state)
    if(length(state$textcollection))
        .doTextNode(parent = sect, state = state)
    parent$addChild(sect)
}

processRdbPara = function(node, parent, xsl = NULL, state, ...)
{
    
    #XXX for now I will assume that r:code is directly under para. This will need to be relaxed in the future...
    hasRcode = length(getNodeSet(node, ".//x:code", namespaces=c(x="http://www.r-project.org"))) > 0
    
    if(!hasRcode)
        state$textcollection <- c(state$textcollection, node)
    else
    {
                                        #XXX Should be using a SAX parser or something else to deal with this whole mess!
        tmp = saveXML(node, )
        tmp = gsub("<r:code>", "</para><r:code>", tmp)
        tmp = gsub("</r:code>", "</r:code><para>", tmp)
        tmp = paste0("<root xmlns:r='http://www.r-project.org'>\n", tmp, "\n</root>")
        tmpnode = xmlRoot(xmlParse(tmp))
        xpathApply(tmpnode, "./*", function(nd)
               {
                   if(xmlName(nd) == "para")
                       state$textcollection <- c(state$textcollection, nd)
                   else if (grepl("code", xmlName(nd)))
                   {
                       .doTextNode(parent = parent, state = state)
                       processRdbCode(nd, parent = parent,  xsl = xsl, in.last.para = TRUE, state = state)
                   }
               })
    }
}

processRdbCode = function(node, parent, xsl, in.last.para = FALSE, state)
{
    code = xmlValue(node)
    
    codel = rCodeElement$new(content = code, formatSpecific = list(rdb = list(in.last.para = in.last.para)))
    
    parent$addChild(codel)
}
