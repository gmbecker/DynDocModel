setMethod("show", "DynDoc",
          function(object)
          {
            cat(paste0("\nAn object of class 'DynDoc'\n\nA Dynamic Document containing\n\t", length(object$elements), " elements\n"))
          })

setMethod("show", "CodeElement",
          function(object)
          {
            cat(paste0("\nAn object of class '", class(object), "'", "\n\nA Code Element:\n\tassociated with ",length(object$outputs), " outputs\n\tOutput formats:  ", unique(sapply(object$outputs, function(o) o$format)), "\n"))
          })
            
