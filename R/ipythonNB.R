
readIPyNotebook = function(filename, rlist = fromJSON(filename), ...)
  {
    fspec = rlist[!(names(rlist) %in% c("metadata", "worksheets"))]
    doc = dynDoc$new(metadata = as.list(rlist$metadata), formatSpecific =as.list(fspec))
    ws = rlist[["worksheets"]][[1]]
    sapply(ws$cells, processEl, parent = doc)
    doc
  }

#TODO I should probably be using S3 method dispatch for this...

processEl = function(el, parent= NULL)
  {
    if(is.null(parent))
      parent = dynDoc$new()
    switch(el$cell_type,
           "code" = codeElFromIPN(el, parent),
           "markdown" = mdElFromIPN(el, parent),
           "raw" = textElFromIPN(el, parent),
           "heading" = , # not sure what to do about this yet sections?
           "altset" = altsetElFromIPN(el, parent),
           "alt" = altElFromIPN(el, parent),
           "interactivecode" = intcodeElFromIPN(el, parent),
           "task" = taskElFromIPN(el,parent),
                                        #This happens in the code, not independently "output" = outputElFromIPN(el, parent),
           stop(paste("Unrecognized cell_type:", el$cell_type))
           )
#    parent
  }

mdElFromIPN = function(el, parent)
{
    mdat = splitMetadata(el$metadata)
    newel = mdElement$new(content=el$source, parent = parent, attributes = mdat$attrs, formatSpecific = list(metadata = mdat$formspec))
    parent$addChild(newel)
    newel
  }

textElFromIPN = function(el, parent)
  {
      mdat = splitMetadata(el$metadata)
    newel = textElement$new(content=el$source, parent = parent, attributes = mdat$attrs, formatSpecific = list(metadata = mdat$formspec))
    parent$addChild(newel)
#    parent
    newel
  }

codeElFromIPN = function(el, parent)
{
    newcode = codeToCodeEl(el, parent)
    if(!is.null(el$outputs))
    {  
        newouts = lapply(el$outputs, outToOutputEl, code = newcode, parent = parent)
        if(!is.list(newouts))
            list(newouts)
        newcode$outputs = as(newouts, "ElementList")
    }
    parent$addChild(newcode) # do we also add newouts?
#    parent
    newcode
  }

intcodeElFromIPN = function(el, parent)
  {
    newcode = intcodeToIntCodeEl(el, parent)
    if(!is.null(el$outputs))
    {  
      newouts = lapply(el$outputs, outToOutputEl, code = newcode, parent = parent)
      if(!is.list(newouts))
        list(newouts)
      newcode$outputs = as(newouts, "ElementList")
    }
    parent$addChild(newcode) # do we also add newouts?
    #parent
    newcode
  }


altsetElFromIPN = function(el, parent)
{
    mdat = splitMetadata(el$metadata)
    newel = branchSetElement$new(parent = parent, attributes = mdat$attrs, formatSpecific = list(metadata = mdat$formspec))
    newel$children = lapply(el$cells, processEl, parent = newel)
    parent$addChild(newel)
#    parent
    newel
  }


altElFromIPN = function(el, parent)
{
    mdat = splitMetadata(el$metadata)     
    newel = branchElement$new(parent = parent, attributes = mdat$attrs, formatSpecific = list(metadata = mdat$formspec))
    newel$children = lapply(el$cells, processEl, parent = newel)
    parent$addChild(newel)
#    parent
    newel
  }

taskElFromIPN = function(el, parent)
{
    mdat = splitMetadata(el$metadata)
    newel = taskElement$new(parent = parent, attributes = mdat$attrs, formatSpecific = list(metadata = mdat$formspec))
    newel$children = lapply(el$cells, processEl, parent = newel)
    parent$addChild(newel)
#    parent
    newel
  }

intcodeToIntCodeEl = function(code, parent)
  {
    

    content = code$input
    language = code$language
    formatSpecific = code[!grepl("(input|outputs|widgets|metadata)", names(code))]
   
    widgets =  as(lapply(code$widgets, IPyNBWidgetToWidget), "WidgetsList")
    if(language == "python" && length(grep("%%R", content)))
      {
        #TODO: need to deal with arguments passed to the rmagic eg push pull etc
        #the first line is the %%R... we need to keep this around so we can put it back, but it shouldn't be in the content(code) for the RCodeElement object
        #formatSpecific$rmagicLine = content[1]
        #content = content[-1]
        #don't include \\n
        if(length(content) == 1)
          {
           # formatSpecific$rmagicLine = gsub("(%%R[^\\n]*)\\n.*", "\\1", content)
              formatSpecific$rmagicLine = gsub("(%%R[^\n]*)\n.*", "\\1", content)
                                        #do include \\n
            content = gsub("%%R[^\n]*\n", "", content)
          } else {
            #if it is on multiple lines the %%R must be the first
            formatSpecific$rmagicLine = content[1]
            content = content[-1]
          }
        language = "R"
      }
        
                                        #formatSpecific = code[!grepl("(input|outputs|language)", names(code))]

    mdat = splitMetadata(code$metadata)
    formatSpecific$metadata = mdat$formspec
    
    constructor = switch(language,
           python = intPyCodeElement$new,
           R = intRCodeElement$new,
           stop(paste("Unrecognised language:", language))
           )
    
    constructor(content=content, parent = parent, formatSpecific=formatSpecific, widgets = widgets, attributes = mdat$attrs)
  }

codeToCodeEl = function(code, parent, interactive = FALSE)
  {
    content = code$input
    if(!length(content))
        content = ""
    
    language = code$language
    formatSpecific = code[!grepl("(input|outputs|metadata)", names(code))]

    if(language == "python" && length(grep("%%R", content)))
      {
        #TODO: need to deal with arguments passed to the rmagic eg push pull etc
        #the first line is the %%R... we need to keep this around so we can put it back, but it shouldn't be in the content(code) for the RCodeElement object
        #formatSpecific$rmagicLine = content[1]
        #content = content[-1]
        #don't include \\n
        if(length(content) == 1)
          {
            formatSpecific$rmagicLine = gsub("(%%R[^\\\n]*)\\\n.*", "\\1", content)
                                        #do include \\n
            content = gsub("%%R[^\\\n]*\\\n", "", content)
          } else {
            #if it is on multiple lines the %%R must be the first
            formatSpecific$rmagicLine = content[1]
            
            content = content[-1]
          }
        language = "R"
      }
        
                                        #formatSpecific = code[!grepl("(input|outputs|language)", names(code))]

    mdat = splitMetadata(code$metadata)

    formatSpecific$metadata = mdat$formspec
        
    constructor = switch(language,
           python = pyCodeElement$new,
           R = rCodeElement$new,
           stop(paste("Unrecognised language:", language))
           )
    
    constructor(content=content, parent = parent, formatSpecific=formatSpecific, attributes = mdat$attrs)
  }

outToOutputEl = function(outel, code, parent)
  {
    if(!is.list(outel))
        outel = as.list(outel)
    content = outel$text
    format = outel$output_type
    metadata = outel$metadata
    formatSpecific = outel[!grepl("(text|output_type|metadata)", names(outel))]

    mdat = splitMetadata(metadata)
    attrs = mdat$attrs
    formatSpecific$metadata = mdat$formspec
    outputElement$new(codeElement = code, parent = parent, content = content, format = format, attributes = attrs, formatSpecific = formatSpecific)
  }


writeIPyNB = function(doc, file = NULL, ...)
  {
    
    if(!is.null(doc$formatSpecific))
      listout = do.call(list, doc$formatSpecific)
    else
      listout=list()
    
    listout$metadata = doc$metadata

    #TODO not handling metadata on worksheets right now.
    listout$worksheets = list(list(cells = lapply(doc$children, renderCellIPyNB)))

    json = toJSON(listout)
    if(!is.null(file))
      cat(json, file=file)
    else
      json
    
  }


splitMetadata = function(meta)
{
    if(is.null(meta) || length(meta) == 0)
        list(attrs = NULL, formspec = NULL)
    attrs = list()
    if(!is.list(meta))
        meta = as.list(meta)
 #   if(!is.null(meta[["id"]]))
  #      attrs$id = meta[["id"]]
  #  if(!is.null(
    attrs = c(id=meta[["id"]], meta[["dyndocmodel"]])
    if(!is.list(attrs))
            attrs = as.list(attrs)
    formspec = meta[-which(names(meta) %in% c("id", "dyndocmodel"))]
    
    list(attrs = attrs, formspec = formspec)
}



