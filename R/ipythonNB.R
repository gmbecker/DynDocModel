
readIPyNotebook = function(filename, rlist = fromJSON(filename))
  {
    fspec = rlist[!(names(rlist) %in% c("metadata", "worksheets"))]
    doc = dynDoc$new(metadata = as.list(rlist$metadata), formatSpecific =as.list(fspec))
    ws = rlist[["worksheets"]][[1]]
    sapply(ws$cells, processEl, parent = doc)
    doc
  }


processEl = function(el, parent= NULL)
  {
    if(is.null(parent))
      parent = dynDoc$new()
    switch(el$cell_type,
           "code" = codeElFromIPN(el, parent),
           "markdown" = mdElFromIPN(el, parent),
           "raw" = textElFromIPN(el, parent),
           "heading" = , # not sure what to do about this yet sections?
           "output" = outputElFromIPN(el, parent),
           stop(paste("Unrecognized cell_type:", el$cell_type))
           )
    parent
  }

mdElFromIPN = function(el, parent)
  {
    newel = mdElement$new(content=el$source, parent = parent, metadata = el$metadata)
    parent$addChild(newel)
    parent
  }

textElFromIPN = function(el, parent)
  {
    newel = textElement$new(content=el$source, parent = parent, metadata = el$metadata)
    parent$addChild(newel)
    parent
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
    parent
  }

codeToCodeEl = function(code, parent)
  {
    content = code$input
    language = code$language
    formatSpecific = code[!grepl("(input|outputs|language)", names(code))]
    constructor = switch(language,
           python = pyCodeElement$new,
           stop(paste("Unrecognised language:", language))
           )
    
    constructor(content=content, parent = parent, formatSpecific=formatSpecific)
  }

outToOutputEl = function(outel, code, parent)
  {
    if(!is.list(outel))
      outel = as.list(outel)
    content = outel$text
    format = outel$output_type
    metadata = outel$metadata
    formatSpecific = outel[!grepl("(text|output_type|metadata)", names(outel))]
    outputElement$new(codeElement = code, parent = parent, content = content, format = format, metadata = metadata, formatSpecific = formatSpecific)
  }


#fromJSON("~/gabe/checkedout/ipython/notebook.ipynb", function(...) browser()
