setMethod("[", "DynDoc",
          function(x, i, j, ...)
          {
            els = x$elements[i, ...]
            as(els, "ElementList")
          })

setMethod("[[<-", "DynDoc",
          function(x, i, ..., value)
          {
            x$elements[[i,...]] = value
            value$posInParent = i
            value$parent = x
            x
          })


setMethod("[[", "DynDoc",
          function(x, i, ...)
          {
            x$elements[[i,...]]
          })

setMethod("[", "ContainerElement",
          function(x, i, j, ...)
          {
            els = x$children[i, ...]
            as(els, "ElementList")
          })


setMethod("[[", "ContainerElement",
          function(x, i, ...)
          {
            x$children[[i,...]]
          })

setMethod("[", "ElementInstance",
          function(x, i, j, ...)
      {
          els = x$children[i, ...]
          as(els, class(x$children))
      })


setMethod("[[", "ElementInstance",
          function(x, i, ...)
          {
            x$children[[i,...]]
          })


setMethod("[", "DocInstance",
          function(x, i, j, ...)
      {
          els = x$children[i, ...]
          as(els, class(x$children))
      })


setMethod("[[", "DocInstance",
          function(x, i, ...)
          {
            x$children[[i,...]]
          })



setMethod("[[<-", "ContainerElement",
          function(x, i, ..., value)
          {
            x$elements[[i,...]] = value
            value$posInParent = i
            value$parent = x
            x
          })



setMethod("[", "ElementList",
          function(x, i, j, ...)
          {
            
            #as(x@.data[i,...], "ElementList")
            as(as(x, "list")[i,...], "ElementList")
          })

setMethod("[[<-", "ElementList",
          function(x, i, ..., value)
          {
            as(x, "list")[[i,...]] = value
            x
            
          })


#setMethod("[[", "ElementList",
#          function(x, i, ...)
#          {
#            as(x, "list")[[i,...]]
#          })
