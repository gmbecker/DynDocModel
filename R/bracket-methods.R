setMethod("[", "DynDoc",
          function(x, i, j, ...)
          {
            els = x$elements[i, ...]
            if(length(els) == 1)
              els[[1]]
            else
              els
          })

setMethod("[", "TaskElement",
          function(x, i, j, ...)
          {
            els = x$children[i, ...]
            if(length(els) == 1)
              els[[1]]
            else
              els
          })
