#A wrapper for Wickham (and Xie)'s evaluator. This captures graphics, warnings, and errors.
#evaluator also captures the code being evaluated and intersperses it, but currently that is now how we construct output files, so we remove those elements in post
if(require(evaluate))
{
    dyndoc_evaluate = function(code, env, ...)
    {
        ret = evaluate(code, env, output_handler = new_output_handler(value = function(x, vis) new("WithVisValue", value = x, visible = vis)))
        #remove echos of the code. We don't want this to show up twice, and right now it would
        ret = ret[sapply(ret, function(x) all(class(x) != "source"))]
        as(ret, "OutputList")
    }
}



setGeneric("evalDynDoc", function(obj, eval = evalWithCache, env = obj$envir,  value = FALSE, ...) standardGeneric("evalDynDoc"))

#what should evalDynDoc return? we need an option to get the actual return/final value but it seems like the machinery works better if it returns the document element

setMethod("evalDynDoc", "DocThread",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              #all the children of a DocThread object will be ElementInstance objects
              for(el in obj$children)
                  {
                      res = evalDynDoc(el, eval = eval, env = env, cache = obj$cacheEngine, value = TRUE, ...)
                      if(is(el$element, "CodeElement")||is(el$element, "ContainerElement"))
                         {
                             if(!is(res, "OutputList"))
                                 res = as(list(res), "OutputList")

                             el$outputs = res
                         } else  {
                             res = new("OutputList")
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
                              res = evalDynDoc(nd, eval=eval, env,value = TRUE, ...)

                            
                              #only instances of code elements get output!
                              if(is(nd$element, "CodeElement"))
                              {
                                  if(!is(res, "OutputList"))
                                      res = as(list(res), "OutputList")
                                  nd$outputs = res
                              } else {
                                  res = new("OutputList")
                              }
                          }
                      if(value)
                          return(nd$outputs)
                      else
                          return(obj)
                  }
              el = obj$element
              args = list(obj = el, eval = eval, env = env, value = TRUE,...)
              if(!"cache" %in% names(args))
                  args$cache = obj$cacheEngine

              res = do.call(evalDynDoc, args)
              #res = evalDynDoc(el, eval, env, cache = obj$cacheEngine ...)
              if(!is(res, "OutputList"))
                  res = as(list(res), "OutputList")
              obj$outputs = res
              if(value)
                  res
              else
                  obj
          })

setMethod("evalDynDoc", "TextElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              if(value)
                  new("OutputList")
              else
                  obj
          })

setMethod("evalDynDoc", "ContainerElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... ){
              res = new("OutputList")
              if(length(obj$children))
              {
                  for(nd in obj$children)
                  {
                      args = list(obj = nd, eval = eval, env = env, ...)
                      if(is(nd, "CodeElement") || is(nd, "ContainerElement")) {
                          res = evalDynDoc(nd, eval, env,value = TRUE, ...)
                          if(!is(res, "OutputList"))
                              res = as(list(res), "OutputList")
                          
                      } else {
                          res = new("OutputList")
                      }
                  }
              }

              if(value)
                  res
              else
                  obj
          })


#evaluating an element (NOT an instance) always evalDynDocs the code and returns the return value, because we never change the underlying element
# only the instance
setMethod("evalDynDoc", "RCodeElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, force, ...)
          {
           
              if(!is.null(obj$attributes$dyndocmodel) && !is.null(obj$attributes$dyndocmodel$cache))
                  force = !obj$attributes$dyndocmodel$cache
              else if (!is.null(obj$attributes$cache))
                  force = !obj$attributes$cache
              else if(missing(force))
                  force = FALSE
              
              res = eval(obj$content, env = env, force = force, ...)
              if(!is(res, "OutputList"))
                  res = as(list(res), "OutputList")
              if(value)
                  res
              else
                  obj
          })

setMethod("evalDynDoc", "PyCodeElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ...)
          {
              warning("not evaluating python code")
              new("OutputList")
          })

setMethod("evalDynDoc", "DynDoc",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
          {
              if(any(sapply(obj$children, function(o) is(o, "BranchElement"))))
                  warning("The document contains branches, which are not yet fully supported")

              thread = getThread(obj, ...)
              evalDynDoc(thread, eval = eval, env = env, ..., value = FALSE)
          })

setMethod("evalDynDoc", "DocElement",
          function(obj, eval = evalWithCache, env = obj$envir, value = FALSE, ... )
      {
          if(value)
              new("OutputList")
          else
              obj
      })
          
