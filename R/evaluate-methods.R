setGeneric("evalDynDoc", function(obj, eval = evalWithCache, env = obj$envir,  value = FALSE, ...) standardGeneric("evalDynDoc"))

#what should evalDynDoc return? we need an option to get the actual return/final value but it seems like the machinery works better if it returns the document element

setMethod("evalDynDoc", "DocThread",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              for(el in obj$children)
                  {
                      if(is(el$element, "CodeElement"))
                         {
                             res = evalDynDoc(el, eval = eval, env = env, cache = obj$cacheEngine, value=TRUE, ...)
                             if(!is(res, "list"))
                                 res = list(res)
                             el$outputs = res
                                        # el = evalDynDoc(el, eval = eval, env = env, ..., value = FALSE)
                         } else {
                             res = NULL
                         }
                  }
              if(value)
                  res
              else
                  obj
          })

setMethod("evalDynDoc", "ElementInstance",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              if(length(obj$children))
                  {
                      for(nd in obj$children)
                          {
                              args = list(obj = nd, eval = eval, env = env, ...)
                              res = evalDynDoc(nd, eval, env,value = value, ...)
                              if(!is(res, "list"))
                                  res = list(res)
                              nd$outputs = list(res)
                          }
                      if(value)
                          return(res)
                      else
                          return(obj)
                  }
              el = obj$element
              args = list(obj = el, eval = eval, env = env, value = value,...)
              if(!"cache" %in% names(args))
                  args$cache = obj$cacheEngine
              res = do.call(evalDynDoc, args)
              #res = evalDynDoc(el, eval, env, cache = obj$cacheEngine ...)
              if(!is(res, "list"))
                  res = list(res)
              obj$outputs = list(res)
              if(value)
                  res
              else
                  obj
          })

setMethod("evalDynDoc", "TextElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              if(value)
                  NULL
              else
                  obj
          })

#evaluating an element (NOT an instance) always evalDynDocs the code and returns the return value, because we never change the underlying element
# only the instance
setMethod("evalDynDoc", "RCodeElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ...)
          {
              res = eval(obj$content, env = env, ...)
              if(value)
                  res
              else
                  obj
          })

setMethod("evalDynDoc", "PyCodeElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ...)
          {
              warning("not evaluating python code")
              NULL
          })

setMethod("evalDynDoc", "DynDoc",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              if(any(sapply(obj$elements, function(o) is(o, "BranchElement"))))
                  warning("The document contains branches, which are not yet fully supported")

              thread = getThread(obj)
              evalDynDoc(thread, eval = eval, env = env, ..., value = FALSE)
          })
