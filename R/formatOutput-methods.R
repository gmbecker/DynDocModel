
if(require(XML))
  {
setMethod("formatOutput", "XMLInternalNode",
          function(object)
          {
            list(content = saveXML(object), format = "xml")
          })

setMethod("formatOutput", "XMLInternalDocument",
          function(object)
          {
            list(content = saveXML(object), format = "xml")
          })


setMethod("formatOutput", "HTMLInternalDocument",
          function(object)
          {
            list(content = saveXML(object), format = "html")
          })
}


setMethod("formatOutput", "ANY",
          function(object)
          {
              printed = capture.output(print(object))
            list(content = printed , format = "text")
          })

setMethod("formatOutput", "recordedplot",
          function(object)
          {
              tpng = tempfile(fileext="png")
              png(tpng)
              redrawPlot(object)
              dev.off()
              stuff = readLines(tpng)
              list(format="data_display", png=stuff)
          })

setMethod("formatOutput", "trellis",
          function(object)
          {
              tpng = tempfile(fileext="png")
              png(tpng)
              print(object)
              dev.off()
              stuff = readLines(tpng)
              list(format="data_display", png=stuff)
          })

if(require(ggplot2))
    {
setMethod("formatOutput", "ggplot",
          function(object)
          {
              tpng = tempfile(fileext="png")
              png(tpng)
              print(object)
              dev.off()
              stuff = readLines(tpng)
              list(format="data_display", png=stuff)
          })
}

setMethod("formatOutput", "NULL",
          function(object)
          {
            list(content = "", format = "text")
          })

