
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
           #This happens in the code, not independently "output" = outputElFromIPN(el, parent),
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
    formatSpecific = code[!grepl("(input|outputs)", names(code))]
    if(language == "python" && length(grep("%%R", content)))
      {
        #TODO: need to deal with arguments passed to the rmagic eg push pull etc
        #the first line is the %%R... we need to keep this around so we can put it back, but it shouldn't be in the content(code) for the RCodeElement object
        #formatSpecific$rmagicLine = content[1]
        #content = content[-1]
        #don't include \\n
        formatSpecific$rmagicLine = gsub("(%%R[^\\n]*)\\n.*", "\\1", content)
        #do include \\n
        content = gsub("%%R[^\\n]*\\n", "", content)
        language = "R"
      }
        
                                        #formatSpecific = code[!grepl("(input|outputs|language)", names(code))]
    
    constructor = switch(language,
           python = pyCodeElement$new,
           R = rCodeElement$new,
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


writeIPyNB = function(doc, file = NULL)
  {
    
    if(!is.null(doc$formatSpecific))
      listout = do.call(list, doc$formatSpecific)
    else
      listout=list()
    
    listout$metadata = doc$metadata

    #not handling metadata on worksheets right now.
    listout$worksheets = list(list(cells = lapply(doc$elements, writeIPyNode)))

    json = toJSON(listout)
    if(!is.null(file))
      cat(json, file=file)
    else
      json
    
  }

setGeneric("writeIPyNode", function(node) standardGeneric("writeIPyNode"))
setMethod("writeIPyNode", "CodeElement",
          function(node)
          {
 
            input = node$content
            formspec = node$formatSpecific
            ##TODO: someday thsi will involve changing the language element to R instead of adding the rmagic in the ipynb node, but not yet
            magic = NULL
            if(is(node, "RCodeElement"))
              {
                if(is.null(formspec) || is.null(formspec$rmagicLine))
                  magic = "%%R"
                else
                  {
                    magic = formspec$rmagicLine
                    formspec = formspec[-grep("magic", names(formspec))]
                  }
             }
           #includes language
            if(!is.null(formspec))
              listout = do.call(list, formspec)
            else
              listout=list()
  

            listout$metadata = node$metadata
            
            listout$cell_type = "code"
            listout$input = paste(c(magic, node$content), collapse="\n")
            listout$outputs = lapply(node$outputs, writeIPyNode)
            listout
          })

setMethod("writeIPyNode", "OutputElement",
          function(node)
          {

            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()
            listout$metadata = node$metadata
            listout$cell_type="output"
            listout$output_type = node$format
            listout$text = paste(node$content, collapse="")
            listout
          })
setMethod("writeIPyNode", "TextElement",
          function(node)
          {

            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()
            
            listout$metadata = node$metadata
            listout$cell_type = if(is(node, "MDTextElement")) "markdown" else "raw"
            listout$source = paste(node$content, collapse="\n")
            listout
          })

setMethod("writeIPyNode", "TaskElement",
          function(node)
          {
            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()
            listout$metadata = node$metadata
            listout$cell_type = "task"
            listout$cells = lapply(node$children, writeIPyNode)
            listout
          })

setMethod("writeIPyNode", "BranchSetElement",
          function(node)
          {
            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()

            listout$metadata = node$metadata
            listout$cell_type = "altset"
            listout$cells = lapply(node$children, writeIPyNode)
            listout
          })

setMethod("writeIPyNode", "BranchElement",
          function(node)
          {
            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()

            listout$metadata = node$metadata
            listout$cell_type = "alt"
            listout$cells = lapply(node$children, writeIPyNode)
            listout
          })


setMethod("writeIPyNode", "IntRCodeElement",
          function(node)
          {
            tmp = as(node, "RCodeElement", strict=TRUE)
            listout = writeIPyNode(tmp)
            listout$cell_type = "interactivecode"
            listout$widgets = lapply(node$widgets, widgetToIPyNBList)            
            listout
          })

setMethod("writeIPyNode", "IntCodeElement",
          function(node)
          {
            tmp = as(node, "CodeElement", strict=TRUE)
            listout = writeIPyNode(tmp)
            listout$cell_type = "interactivecode"
            listout$widgets = lapply(node$widgets, widgetToIPyNBList)            
            listout
          })

          

