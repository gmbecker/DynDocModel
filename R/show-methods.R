setMethod("show", "DynDoc",
          function(object)
          {
            cat(paste0("\nAn object of class 'DynDoc'\n\nA Dynamic Document containing\n\t", length(object$elements), " elements\n"))
          })

setMethod("show", "DocElement",
          function(object)
          {
            cat(paste0("\nAn object of class ", class(object), "\n",
                       "\n\tparent is of class ", class(object$parent),
                       "\n\tposition in parent: ", object$posInParent, "\n"))
          })
setMethod("show", "ContainerElement",
          function(object)
          {
            cat(paste0("\nAn object of class ", class(object), "\n",
                       "\tcontaining ", length(object$children), " children\n",
                       "\n\tparent is of class ", class(object$parent),
                       "\n\tposition in parent: ", object$posInParent, "\n"))
          })

setMethod("show", "CodeElement",
          function(object)
          {
            cat(paste0("\nAn object of class '", class(object), "'", "\n\nA Code Element:\n\tassociated with ",length(object$outputs), " outputs\n\tOutput formats:  ", unique(sapply(object$outputs, function(o) o$format)),
                       "\n\tparent is of class ", class(object$parent),
                       "\n\tposition in parent: ", object$posInParent,"\n"))
          })
            
