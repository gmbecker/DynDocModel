
#convenience function so I can have the right default driver functions
dyndoc_rpath = function(obj, path, names_fun = dyndoc_rpath_abbrevs, term_condition = dyndoc_rpath_term, attr_fun = dyndoc_rpath_attr)
{
    rpath(obj, path, names_fun = names_fun, term_condition = term_condition,attr_fun = attr_fun)
}

dyndoc_rpath_classes = function(obj)
{

    #for attributes/formatSpecific
    if(is(obj, "list") || is(obj, "character"))
    {
        if(length(obj) && length(names(obj)))
            return(names(obj))
        else
            return(as.character(seq(along = obj)))
    }
    
    if(is(obj, "DynDoc"))
        kids = obj$elements
    else if(any(sapply( c( "DocInstance", "DocThread", "ContainerElement"), function(cl) is(obj, cl))))
        kids = obj$children
    else
        kids = list()
    res = sapply(kids, function(x) if(is(x, "ElementInstance")) class(x$element) else class(x))
    res
}

dyndoc_rpath_abbrevs = function(obj)
{
    doRevAbbrevType(dyndoc_rpath_classes(obj))
}

dyndoc_rpath_term = function(obj)
{
    length(dyndoc_rpath_abbrevs(obj)) > 1
}


setGeneric("dyndoc_rpath_attr", function(obj) standardGeneric("dyndoc_rpath_attr"))

setMethod("dyndoc_rpath_attr", "RCodeElement",
          function(obj)
      {
          c(obj$attributes, obj$formatSpecific, position = posInParent, id = obj$id, invars = obj$invars, outvars = obj$outvars, content = obj$content)
      })

setMethod("dyndoc_rpath_attr", "DocElement",
          function(obj)
      {
          c(obj$attributes, obj$formatSpecific, position = posInParent, id = obj$id)
      })

setMethod("dyndoc_rpath_attr", "TextElement",
          function(obj)
      {
          c(obj$attributes, obj$formatSpecific, position = posInParent, id = obj$id, content = obj$content)
      })

setMethod("dyndoc_rpath_attr", "ElementInstance",
          function(obj)
      {
          ret = dyndoc_rpath_attr(obj$element)
          ret$outputs = obj$outputs
          ret$position = obj$posInParent
      })

setMethod("dyndoc_rpath_attr", "ANY", function(obj) list())

    
