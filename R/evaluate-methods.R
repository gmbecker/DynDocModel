setGeneric("evaluate", function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ...) standardGeneric("evaluate"))

setMethod("evaluate", "DocThread",
          function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ..., value = FALSE)
          {
              for(el in obj@children)
                  {
                     # res = evaluate(el, eval = eval, env = env, ...)
                     # if(!is(res, "list"))
                     #     res = list(res)
                     # el$outputs = res
                      el = evaluate(el, eval = eval, env = env, ..., value = FALSE)
                  }
              if(value)
                  res
              else
                  obj
          })

setMethod("evaluate", "ElementInstance",
          function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ..., value = FALSE)
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
              obj$outputs = list(res)
              if(value)
                  res
              else
                  obj
          })

setMethod("evaluate", "TextElement",
          function(obj, ...)
      {
          obj
      })

setMethod("evaluate", "RCodeElement",
          function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ...)
      {
          eval(obj$content, env = env, ...)
      })

setMethod("evaluate", "DynDoc",
          function(obj, eval = evalWithCache, env = new.env(), formatters = list(), ..., value = FALSE)
          {
              if(any(sapply(obj$elements, function(o) is(o, "BranchElement"))))
                  warning("The document contains branches, which are not yet fully supported")

              thread = getThread(obj)
              evaluate(thread, eval = eval, env = env, formatters = formatters, ..., value = FALSE)
          })
