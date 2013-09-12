#raw and markdown text gets spit out as is
setMethod("renderCellMD", "TextElement",
          function(node, formatters, state, inline = FALSE, ...)
      {
          ret = node$content
          if(!inline)
              ret = paste(c("\n", ret), collapse = "\n")
          ret
      })


 setMethod("renderCellMD", "LatexTextElement",
           function(node, formatters, state,  ...)
       {
           stop("don't know how to transform latex into markdown")
       })

setMethod("renderCellMD", "DbTextElement",
          function(node, formatters, state, ...)
      {
          stop("don't know how to transform docbook into markdown.")
      })

setMethod("renderCellMD", "RCodeElement",
          function(node, formatters, state, doOutput = FALSE, ...)
      {
          ret = paste(c("\n\n```r", node$content, "```"), collapse="\n")
          if(doOutput && length(node$outputs))
          {
              ret = c(ret, sapply(node$outputs, function(x) mdHandleFormatted(formatObject(x, formatters = formatters), inline = FALSE, state = state)))
          }
          paste(ret, collapse = "\n")
      })

setMethod("renderCellMD", "InlineRCode",
          function(node, formatters, state, doOutput = TRUE, ...)
      {
          
          ret = character();
          if(doOutput && length(node$outputs))
          {
              ret = sapply(node$outputs, function(x) mdHandleFormatted(formatObject(x, formatters = formatters), inline = TRUE, state = state))
           }
          paste(ret, collapse = "")
      })


setMethod("renderCellMD", "MixedMDElement",
          function(node, formatters, state, inline = FALSE, ...)
      {
          ret = paste(sapply(node$children, renderCellMD, formatters = formatters, inline = TRUE, state = state, ...), collapse = "")
          if(!inline)
              ret = paste0("\n\n", ret)
          ret
      })



setMethod("renderCellMD", "ElementInstance",
          function(node, formatters, state, inline = FALSE, ...)
      {
          formatters = combineFormatters(node$formatters, formatters)
          
          ret = character()

          if(inline)
              lead = ""
          else
              lead="\n\n"
          
          if(length(node$children))
          {
              if(is(node$element, "MixedTextElement"))
                  inline2 = TRUE
              else
                  inline2 = FALSE

              if(inline2)
                  kbump = ""
              else
                  kbump = "\n\n"
              kidout = paste(paste0(kbump, sapply(node$children, renderCellMD, formatters = formatters, inline = inline2, state = state, ...)), collapse = "\n")
              ret = c(ret, kidout)
          }
          else
          {
              ret = c(ret, renderCellMD(node$element, formatters, inline = inline, state = state, ...))
          }

          if(length(node$outputs))
          {
              if(inline)
                  bumper = "`"
              else
                  bumper = "\n```\n"
              #fout = paste0(bumper, paste(sapply(node$outputs, function(x) formatObject(x, formatters = formatters)@value), collapse = "\n"), bumper)
              fout = unlist(sapply(node$outputs, function(x) mdHandleFormatted(formatObject(x, formatters = formatters), inline = inline, state = state)))
              ret = c(ret, fout)
          }
          ret = unlist(ret, recursive = TRUE)
          ret = paste0(lead, ret)
          ret = paste(ret, collapse ="")
          ret
      })

mdHandleFormatted = function(fout, inline, state)
{
    if(is(fout, "FormattedOutputList"))
        return(unlist(sapply(fout, mdHandleFormatted, inline = inline, state = state)))
    if(inline)
        bumper = "`"
    else
        bumper = "\n```\n"
    switch(fout@format,
           image_data = doImage(fout, state = state),
           null = character(),
           if(length(fout@value)) paste0(bumper, fout@value, bumper) else character()
           )
}

doImage = function(fobject, state, dataURI = FALSE)
{
    #XXX this looks like it is working upon inspection, but then the image doesn't load properly
    if(dataURI){
        tfile = tempfile()
        writeBin(fobject@value, tfile)
        img(tfile)
    } else {
        #XXX come up with a better naming scheme!!!!
        imdir = file.path(state$outdir, "images")

        if(!file.exists(imdir))
            dir.create(imdir, recursive = TRUE)
        fname = file.path(imdir, paste0(state$basePlotName, state$plots, ".", fobject@info$format))
        state$plots = state$plots + 1
        writeBin(fobject@value, fname)
        paste0("<img src='", fname, "' alt='R plot'/>")
    }
 
}
