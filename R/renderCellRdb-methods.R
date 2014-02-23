setMethod("renderCellRdb", "TextElement",
          function(node, formatters, state, converters = list(), inline = FALSE, doOutupt= FALSE, ...)
      {
          if(is(node, "DbTextElement"))
              return(node$content)
          
    
          convCont <- switch(class(node),
                             "MDTextElement" = convertContent(node$content, "markdown", "docbook", converters),
                             "TextElement" = convertContent(node$content, "raw", "docbook"),
                             "LatexTextElement" = convertContent(node$content, "latex", "docbook", converters),
                             stop("unrecognized text element type: ", class(node))
                             )

          #for safety. Some stuff might be coming in in UTF-8, which breaks xmlParse
          if(is(convCont, "character"))
          {
              convCont = iconv(convCont)
              convCont = xmlParse(convCont)
          }
          convCont
      })

setMethod("renderCellRdb", "ContainerElement",
          function(node, formatters, state, converters = list(), inline = FALSE,  ...)
      {
          nd = newXMLNode(getRdbTag(node), attrs = c(node$attributes, node$formatSpecific$rdb))
          kids = lapply(node$children, renderCellRdb, formatters = formatters, state = state, converters = converters, inline = FALSE, ...)
          addChildren(nd, kids = kids)
          nd
       })

setMethod("renderCellRdb", "MixedTextElement",
          function(node, formatters, state, converters = list(), inline = FALSE,  ...)
      {
                nd = newXMLNode(getRdbTag(node), attrs = c(node$attributes, node$formatSpecific$rdb))
          addChildren(nd, kids = lapply(node$children, renderCellRdb, formatters = formatters, state = state, converters = converters, inline = TRUE, ...))
          nd


      })

          

setMethod("renderCellRdb", "PyCodeElement",
          function(node, formatters, doOutput = FALSE, ...)
      {
          
          if(node$content == "%load_ext rmagic")
              return(NULL)
          else
              newXMLNode("py:code", paste(node$content, collapse="\n"), cdata = TRUE)
      })


setMethod("renderCellRdb", "RCodeElement",
          function(node, formatters, doOutput = FALSE, inline = FALSE, ...)
      {
          attrs = node$attributes
          if(length(node$formatSpecific$rdb))
              attrs = c(attrs, node$formatSpecific$rdb)
          if(!inline)
          {
              code = newXMLNode("r:code", paste(node$content , collapse="\n"), cdata = TRUE, attrs = attrs)
              if(doOutput && length(node$outputs))
              {
                  for(o in node$outputs)
                  {
                      RdbHandleFormatted(formatObject(o, formatters = formatters, inline = FALSE, state = state))
                  }
              }
          }else {
              #inline case. We're writing to Rdb, so the code has to still be there, not replaced by output as in rendered view.
              code = newXMLNode("r:expr", node$content, cdata = TRUE, attrs = attrs)}
          code
              
      }) 

setMethod("renderCellRdb", "MixedMDElement",
          function(node, formatters, state, inline = FALSE, doOutput= TRUE, ...)
      {
          sapply(node$children, renderCellRdb, formatters = formatters, inline = TRUE, state = state, ...)
      })




RdbHandleFormatted = function(fout, inline, state, node)
{
    if(is(fout, "FormattedOutputList"))
        return(unlist(sapply(fout, RdbHandleFormatted, inline = inline, state = state, node)))
    switch(fout@format,
           image_data = dbDoImage(fout, state = state, node),
           null = NULL,
           if(length(fout@value)) newXMLNode("r:output", fout@value, cdata=TRUE, parent = node))
    NULL
    #They are xml nodes so the changes happen in place
}

dbDoImage = function(fobject, state, code)
{
                                        #XXX this looks like it is working upon inspection, but then the image doesn't load properly
                                        #XXX come up with a better naming scheme!!!!
    if(is.null(state$outdir))
    {
        warning("Inline images not supported for formatting into Rdb")
        return(NULL)
    }
    imdir = file.path(state$outdir, "images")
    if(!file.exists(imdir))
        dir.create(imdir, recursive = TRUE)
    fname = file.path(imdir, paste0(state$basePlotName, state$plots, ".", fobject@info$format))
    state$plots = state$plots + 1
    writeBin(fobject@value, fname)

    newXMLNode("r:output", newXMLNode("mediaobject", newXMLNode(imageobject, newXMLNode("imagedata", attrs = list(fileref = fname, format = fobject@info$format)))), parent = code)
    #xml nodes so the changes happen in place.
    NULL
}



setMethod("getRdbTag", "TaskElement", function(el) "task")
setMethod("getRdbTag", "DecisionElement", function(el) "decision")
setMethod("getRdbTag", "AltElement", function(el) "alternative")
setMethod("getRdbTag", "AltImplElement", function(el) "altImplementation")
setMethod("getRdbTag", "AltMethodElement", function(el) "altApproach")
setMethod("getRdbTag", "AltImplSetElement", function(el) "altImplementations")
setMethod("getRdbTag", "AltMethodSetElement", function(el) "altApproaches")
