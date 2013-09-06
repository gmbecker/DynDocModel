setMethod("renderCellIPyNB", "CodeElement",
          function(node, formatters, ...)
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
  

            listout$metadata = node$attributes
            
            listout$cell_type = "code"
            listout$input = paste(c(magic, node$content), collapse="\n")
            listout$outputs = lapply(node$outputs, renderCellIPyNB, formatters = formatters, ...)
            listout
          })

setMethod("renderCellIPyNB", "OutputElement",
          function(node, formatters, ...)
          {

            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()
            listout$metadata = node$attributes
            listout$cell_type="output"
            listout$output_type = node$format
            listout$text = paste(node$content, collapse="")
            listout
          })
setMethod("renderCellIPyNB", "TextElement",
          function(node, formatters, ...)
          {

            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()
            
            listout$metadata = node$attributes
            listout$cell_type = if(is(node, "MDTextElement")) "markdown" else "raw"
            listout$source = paste(node$content, collapse="\n")
            listout
          })

setMethod("renderCellIPyNB", "TaskElement",
          function(node, formatters, ...)
          {
            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()
            listout$metadata = node$attributes
            listout$cell_type = "task"
            listout$cells = lapply(node$children, renderCellIPyNB, formatters = formatters, ...)
            listout
          })

setMethod("renderCellIPyNB", "BranchSetElement",
          function(node, formatters, ...)
          {
            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()

            listout$metadata = node$attributes
            listout$cell_type = "altset"
            listout$cells = lapply(node$children, renderCellIPyNB, formatters = formatters, ...)
            listout
          })

setMethod("renderCellIPyNB", "BranchElement",
          function(node, formatters, ...)
          {
            if(!is.null(node$formatSpecific))
              listout = do.call(list, node$formatSpecific)
            else
              listout=list()

            listout$metadata = node$attributes
            listout$cell_type = "alt"
            listout$cells = lapply(node$children, renderCellIPyNB, formatters = formatters, ...)
            listout
          })


setMethod("renderCellIPyNB", "IntRCodeElement",
          function(node, formatters, ...)
          {
            tmp = as(node, "RCodeElement", strict=TRUE)
            listout = renderCellIPyNB(tmp, formatters = formatters, ...)
            listout$cell_type = "interactivecode"
            listout$widgets = lapply(node$widgets, widgetToIPyNBList)            
            listout
          })

setMethod("renderCellIPyNB", "IntCodeElement",
          function(node, formatters, ...)
          {
            tmp = as(node, "CodeElement", strict=TRUE)
            listout = renderCellIPyNB(tmp, formatters = formatters, ...)
            listout$cell_type = "interactivecode"
            listout$widgets = lapply(node$widgets, widgetToIPyNBList)            
            listout
          })


setMethod("renderCellIPyNB", "ElementInstance",
          function(node, formatters, ...)
          {
              formatters = combineFormatters(node$formatters, formatters)
              
              if(!is.null(node$element$formatSpecific) && !is.null(node$element$formatSpecific$ipynb))
                  listout = do.call(list, node$element$formatSpecific$ipynb)
              else
                  listout=list()
              
              listout$metadata = node$element$attributes
              listout$cell_type = getIPyCellType(node$element)
              
              if(length(node$children))
                  {
                      listout$cells = lapply(node$children, renderCellIPyNB, formatters = formatters, ...)
                  }
              else
                  {
                      listout = c(listout, renderCellIPyNB(node$element, formatters, ...))
                      listout = listout[!duplicated(names(listout))]
                  }
              #XXX clobber the outputs with properly formatted ones. This is wasteful but should work for now without major infrastructure changes.
              if(length(node$outputs))
                  listout$outputs = lapply(node$outputs, formatObject, formatters = formatters)
              listout
          })

setMethod("getIPyCellType", "TaskElement", function(el) "task")
setMethod("getIPyCellType", "BranchSetElement", function(el) "altset")
setMethod("getIPyCellType", "BranchElement", function(el) "alt")
setMethod("getIPyCellType", "TextElement", function(el) "raw")
setMethod("getIPyCellType", "MDTextElement", function(el) "markdown")
setMethod("getIPyCellType", "DbTextElement", function(el) stop("translation of Docbook into markdown/raw text not yet implemented"))
setMethod("getIPyCellType", "MixedTextElement", function(el) stop("mixed content cell types are not (yet) supported in IPython Notebook"))
setMethod("getIPyCellType", "CodeElement", function(el) "code")
setMethod("getIPyCellType", "IntCodeElement", function(el) "interactivecode")
setMethod("getIPyCellType", "IntCodeElement", function(el) "interactivecode")
