
if(require(XML))
  {
setMethod("formatOutput", "XMLInternalNode",
          function(object)
          {
            new("FormattedOutput", value = saveXML(object), format = "xml")
          })

setMethod("formatOutput", "XMLInternalDocument",
          function(object)
          {
            new("FormattedOutput", value = saveXML(object), format = "xml")
          })


setMethod("formatOutput", "HTMLInternalDocument",
          function(object)
          {
            new("FormattedOutput", value = saveXML(object), format = "html")
          })
}


setMethod("formatOutput", "ANY",
          function(object)
          {
              printed = capture.output(print(object))
            new("FormattedOutput", value = printed , format = "text")
          })

setMethod("formatOutput", "WithVisValue",
          function(object)
      {
          if(object@visible)
              printed = capture.output(print(object@value))
          else
              printed = character()
          new("FormattedOutput", value = printed, format = "text")
      })


setMethod("formatOutput", "WithVisPlusGraphics",
          function(object)
      {
          if(object@visible)
              printed = capture.output(print(object@value))
          else
              printed = character()
          
          val = new("FormattedOutput", value = printed, format = "text")
          graphics = lapply(object@graphics, formatOutput)
          as(unlist(list( graphics, val), recursive = FALSE), "FormattedOutputList")
      })

setMethod("formatOutput", "recordedplot",
          function(object)
          {
              .fimage(object, redrawPlot)
          })

setMethod("formatOutput", "trellis",
          function(object)
          {
              .fimage(object)
          })

if(require(ggplot2))
    {
setMethod("formatOutput", "ggplot",
          function(object)
          {
              .fimage(object)
          })
}

setMethod("formatOutput", "NULL",
          function(object)
          {
            new("FormattedOutput", value = character(), format = "text")
          })

.fimage <- function(obj, disp_fun = print)
{
    tpng = tempfile(fileext="png")
    png(tpng)
    disp_fun(obj)
    dev.off()
    stuff = readBin(tpng, raw(), n = file.info(tpng)$size)
    new("FormattedOutput", format="image_data", value=stuff, info = list(format = "png"))
}    
