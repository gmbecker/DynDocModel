
# Possible ways to allow multiple evaluated instances of the same document E.g. from evaluating different threads
# through it, or with different input values
#
# Switch to S4 classes entirely, no inherent connection between the same node within different instances
#
# ** Have wrapper S4 classes that carry around a referenceClass DocElement object, but can point to different parents and hang onto output, etc
#



parseEval = function(code = NULL, file =  "")
  {
    eval(parse(text = code))
  }

default_eval = list(R = parseEval,
  python = function(...)
  {
    warn("there is no default parser for python yet")
    NULL
  })

  getFormatter = function(obj, formatters)
  {
    if(!missing(formatters) && (any( class(res) %in% names(formatters))))
      {
        ind = which(names(formatters) %in% class(res))
        formatter = formatters[[ind]]
        
      } else if ("ANY" %in% names(formatters)) {
        formatter = formatters[["ANY"]]
      } else {
        formatter = formatOutput
      }
    formatter
   }

getEvaluator = function(element, evaluators)
  {
    lang = ""
    ret = NULL
    if(class(element) %in% names(evaluators))
      ret = evaluators[[class(element)]]
    else if(is(element, "RCodeElement"))
      lang = "R"
    else if (is(element, "PyCodeElement"))
      lang = "python"
    
    if(nchar(lang))
      {
        if(lang %in% names(evaluators))
          ret = evaluators[[lang]]
        else
          ret = default_eval[[lang]]
      }
    if(is.null(ret))
      stop(paste("Unable to find evaluator for element of class", class(element)))
    ret
  }

runDynDoc = function(doc,
  evaluators = list(),
  formatters = list(),
  subdocument = NULL,
  caching = FALSE,
  out = NULL,
  output.format = if(!is.null(file)) tolower(gsub(".*\\.(*$)", "\\1", file)) else NULL,
  renderer = if(!is.null(format)) DefaultRenders[[tolower(format)]] else getDefaultRenderer(file, ...)
  )
  {

    rootenv = new.env()
    sapply(doc$elements, runElement, evaluators = evaluators, formatters = formatters, parent.env = rootenv)
    if(!is.null(out))
      invisible(writeDynDoc(doc, file = out, format = output.format, renderer = renderer))
    }

setMethod("runElement", "RCodeElement",
          function(el, evaluators, parent.env,  formatters, ...)
          {
            env = new.env(parent = parent.env)
            code = el$contents
            eval = evaluators[["R"]]
            res = eval(code, env = env, ...)
            formatter = getFormatter(res, formatters)
            out = formatter(res, ...)
            newout = outputElement$new(codeElement = el, format = out$format, content = out$content)
            el$outputs = as(c(el$outputs, newout), "ElementList")
          })

setMethod("runElement", "PyCodeElement",
          function(el, evaluators, parent.env,  ...)
          {
            env = new.env(parent = parent.env)
            code = el$contents
            eval = evaluators[["python"]]
            res = eval(code, env = env, ...)
            if(!is.null(res))
              {
                newout = outputElement$new(codeElement = el, format = "RObject", content = res)
                el$outputs = as(c(el$outputs, newout), "ElementList")
              }
          })


setMethod("runElement", "ContainerElement",
          function(el, evaluators, parent.env, ...)
          {
            env = new.env(parent = parent.env)
            sapply(el$children, runElement, evaluators = evaluators, parent.env = env, ...)
          })

          
setMethod("runElement", "TextElement",
          function(el, evaluators, parent.env, ...)
          {
            NULL
          })
