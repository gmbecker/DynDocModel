
if(require(XML))
  {
setMethod("formatOutput", "XMLInternalNode",
          function(object, formatters = list())
          {
            new("FormattedOutput", value = saveXML(object), format = "xml")
          })

setMethod("formatOutput", "XMLInternalDocument",
          function(object, formatters = list())
          {
            new("FormattedOutput", value = saveXML(object), format = "xml")
          })


setMethod("formatOutput", "HTMLInternalDocument",
          function(object, formatters = list())
          {
            new("FormattedOutput", value = saveXML(object), format = "html")
          })
}


setMethod("formatOutput", "ANY",
          function(object, formatters = list())
          {
              printed = capture.output(print(object))
              new("FormattedOutput", value = paste(printed, collapse="\n") , format = "text")
          })

setMethod("formatOutput", "WithVisValue",
          function(object, formatters = list())
      {
          if(object@visible)
              formatObject(object@value, formatters=formatters)
          else
              new("FormattedOutput", value = character(), format = "null")
      })


setMethod("formatOutput", "WithVisPlusGraphics",
          function(object, formatters = list())
      {
          if(object@visible)
              val = formatObject(object@value, formatters=formatters)
          else
              val = new("FormattedOutput", value = character(), format = "null")
          graphics = lapply(object@graphics, formatObject, formatters = formatters)
          if(length(graphics))
              val = c(graphics, val)
          if(!is(val, "list"))
              val = list(val)
          as(val, "FormattedOutputList")
      })

setMethod("formatOutput", "recordedplot",
          function(object, formatters = list())
          {
              .fimage(object, redrawPlot)
          })

setMethod("formatOutput", "trellis",
          function(object, formatters = list())
          {
              .fimage(object)
          })

if(require(ggplot2))
    {
setMethod("formatOutput", "ggplot",
          function(object, formatters = list())
          {
              .fimage(object)
          })
}


setMethod("formatOutput", "PlotList",
          function(object, formatters = list())
      {
          ret = lapply(object, formatObject, formatters = formatters)
          as(ret, "FormattedOutputList")
      })

setMethod("formatOutput", "OutputList",
          function(object, formatters = list())
      {
          ret = lapply(object, formatObject, formatters = formatters)
          as(ret, "FormattedOutputList")
      })


#setMethod("formatOutput", "NULL",
#          function(object)
#          {
#            new("FormattedOutput", value = character(), format = "text")
#          })

.fimage <- function(obj, disp_fun = print)
{
    tpng = tempfile(fileext="png")
    png(tpng)
    disp_fun(obj)
    dev.off()
    stuff = readBin(tpng, raw(), n = file.info(tpng)$size)
    new("FormattedOutput", format="image_data", value=stuff, info = list(format = "png"))
}    
