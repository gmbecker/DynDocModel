setMethod("renderCellIPyNB", "CodeElement",
          function(node, formatters, doOutputs = TRUE, ...)
          {
 
            input = list(node$content)
            if("ipynb" %in% names(node$formatSpecific))
                formspec = node$formatSpecific$ipynb
            else
                formspec = NULL
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
  

            listout$metadata = ipynbMetadata(node$attributes)
            
            listout$cell_type = "code"
            listout$language = "python"
            listout$input = paste(c(magic, node$content), collapse="\n")
            if(doOutputs)
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
            listout$metadata = ipynbMetadata(node$attributes)
            listout$cell_type="output"
            listout$output_type = node$format
            listout$text = paste(node$content, collapse="")
            listout
          })

setMethod("renderCellIPyNB", "TextElement",
          function(node, formatters, converters = list(), ...)
          {

            if(!is.null(node$formatSpecific))
                listout = do.call(list, node$formatSpecific)
            else
                listout=list()
            
            listout$metadata = ipynbMetadata(node$attributes)
            convCont <- switch(class(node),
                               "MDTextElement" = node$content,
                               "TextElement" = node$content,
                               "DbTextElement" = convertContent(node$content, "docbook", "markdown", converters),
                               "LatexTextElement" = convertContent(node$content, "latex", "markdown", converters),
                               stop("unrecognized text element type: ", class(node))
                               )
            
            #IPython Notebook does all it's encoding in UTF-8
#            convCont = iconv(convCont, to="UTF-8")
            #It also uses fancy opening and closing doublequotes.
 #           convCont = insertFancyQuotes(convCont)
            
            if(class(node) == "TextElement")
                type = "raw"
            else
                type = "markdown"
            listout$cell_type = type
            
#            browser()
    #        listout$source = list(paste(convCont, collapse = "\n"))
            listout$source = convCont
            listout
          })

setMethod("renderCellIPyNB", "MixedMDElement",
          function(node, formatters, ...)
      {
          warning("IPython Notebook does not currently support mixed prose/code cells. Treating cell as pure markup")
          
           if(!is.null(node$formatSpecific) && !is.null(node$formatSpecific$ipynb))
              listout = do.call(list, node$formatSpecific$ipynb)
            else
              listout=list()
          listout$metadata = ipynbMetadata(node$attributes)
          listout$cell_type = "markdown"
          listout$content = paste(sapply(node$children,
                                          function(x) {
                                              if(is(x, "InlineRCode"))
                                                  paste0("`r ", x$content, "`")
                                              else
                                                  x$content
                                          }), collapse = " ")
          listout
      })

setMethod("renderCellIPyNB", "TaskElement",
          function(node, formatters, ...)
          {
              if(!is.null(node$formatSpecific) && !is.null(node$formatSpecific$ipynb))
                  listout = do.call(list, node$formatSpecific$ipynb)
              else
                  listout=list()
            
            listout$metadata = ipynbMetadata(node$attributes)
            listout$cell_type = "task"
            listout$cells = lapply(node$children, renderCellIPyNB, formatters = formatters, ...)
            listout
          })

setMethod("renderCellIPyNB", "DecisionElement",
          function(node, formatters, ...)
          {
              if(!is.null(node$formatSpecific) && !is.null(node$formatSpecific$ipynb))
                  listout = do.call(list, node$formatSpecific$ipynb)
              else
                  listout=list()

              listout$metadata = ipynbMetadata(node$attributes)
              listout$cell_type = "altset"
              listout$cells = lapply(node$children, renderCellIPyNB, formatters = formatters, ...)
              listout
          })

setMethod("renderCellIPyNB", "AltElement",
          function(node, formatters, ...)
          {
              if(!is.null(node$formatSpecific) && !is.null(node$formatSpecific$ipynb))
                  listout = do.call(list, node$formatSpecific$ipynb)
              else
                  listout=list()

              listout$metadata = ipynbMetadata(node$attributes)
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

              #XXX We'll need to do better once I put mixed cells into IPython notebook
              if(length(node$children) && !is(node$element, "MixedTextElement" ))
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
              {
                  listout$outputs = lapply(node$outputs, function(x) ipynbHandleFormatted(formatObject(x, formatters = formatters)))
                  listout$outputs = listout$outputs[sapply(listout$outputs, function(x) length(x) > 0)]
              }
              listout
          })




setMethod("getIPyCellType", "TaskElement", function(el) "task")
setMethod("getIPyCellType", "DecisionElement", function(el) "altset")
setMethod("getIPyCellType", "AltElement", function(el) "alt")
setMethod("getIPyCellType", "TextElement", function(el) "raw")
setMethod("getIPyCellType", "MDTextElement", function(el) "markdown")
setMethod("getIPyCellType", "DbTextElement", function(el) stop("translation of Docbook into markdown/raw text not yet implemented"))
setMethod("getIPyCellType", "MixedTextElement", function(el) stop("mixed content cell types are not (yet) supported in IPython Notebook"))
setMethod("getIPyCellType", "MixedMDElement", function(el) "markdown")
setMethod("getIPyCellType", "CodeElement", function(el) "code")
setMethod("getIPyCellType", "IntCodeElement", function(el) "interactivecode")
setMethod("getIPyCellType", "IntCodeElement", function(el) "interactivecode")

ipynbHandleFormatted = function(fout)
{
    if(is(fout, "FormattedOutputList"))
        return(unlist(lapply(fout, ipynbHandleFormatted), recursive = FALSE))

    switch(fout@format,
           text = list(metadata = emptyNamedList, output_type = "display_data", text = list(fout@value)),
           image_data = list(metadata=emptyNamedList, output_type = "display_data", png = ipynbDoPNG(fout)),
           null = emptyNamedList)

}

ipynbDoPNG = function(fout)
{
    if(!(fout@info$format == "png"))
        stop("This image is not a png! other image formats are not yet supported")
    #base64encode(fout@value)
    tfile = tempfile()
    writeBin(fobject@value, tfile)
    img(tfile)
}

ipynbMetadata = function(attrs)
{
    if(length(attrs))
        attrs
    else
        emptyNamedList
}
