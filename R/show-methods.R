setMethod("show", "DynDoc",
          function(object)
          {
            cat(paste0("\nAn object of class 'DynDoc'\n\nA Dynamic Document containing\n\t", length(object$children), " elements\n"))
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

setMethod("show", "DocInstance",
          function(object)
      {
          cat(paste0("\nAn object of class ", class(object), "\n",
                     "\n\tContains Instances of ", length(object$children), " document elements.\n"))
      })

setMethod("show", "ElementInstance",
          function(object)
      {
          cat(paste0("\nAn object of class ", class(object), "\n",
                     "\n\tIt is an Instance of a ", class(object$element), " document element.\n",
                     "\tHas ", length(object$outputs), " associated output objects.\n",
                     "\tContains Instances of ", length(object$children), " child elements\n"))
      })

