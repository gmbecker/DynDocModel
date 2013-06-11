
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
            txtcon = textConnection("printed", "w")
            sink(txtcon)
            print(object)
            sink()
            list(content = printed , format = "text")
          })

setMethod("formatOutput", "NULL",
          function(object)
          {
            list(content = "", format = "null")
          })

