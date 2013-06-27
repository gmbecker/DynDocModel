setGeneric("evaluate", function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ...) standardGeneric("evaluate"))

setMethod("evaluate", "DocThread",
          function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ...)
          {
              for(el in obj@children)
                  {
                      res = evaluate(el, eval = eval, env = env, ...)
                      if(!is(res, "list"))
                          res = list(res)
                      el$outputs = res
                  }
              res
          })

setMethod("evaluate", "ElementInstance",
          function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ...)
          {
              if(length(obj$children))
                  {
                      for(nd in obj$children)
                          {
                              res = evaluate(nd, eval, env, formatters, ...)
                              nd$outputs = list(res)
                          }
                      return(res)
                  }
              el = obj$element
              res = evaluate(el, eval, env, formatters, ...)
              obj@outputs = list(res)
              res
          })
