
#convenience function so I can have the right default driver functions
dyndoc_rpath = function(obj, path, names_fun = dyndoc_rpath_abbrevs, term_condition = dyndoc_rpath_term, attr_fun = dyndoc_attrs)
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
    classToAbbrev(dyndoc_rpath_classes(obj))
}

dyndoc_rpath_abbrevs2 = function(obj)
{
    classToAbbrev2(dyndoc_rpath_classes(obj))
}


dyndoc_rpath_term = function(obj)
{
    length(dyndoc_rpath_abbrevs(obj)) > 1
}


setGeneric("dyndoc_attrs", function(obj) standardGeneric("dyndoc_attrs"))

setMethod("dyndoc_attrs", "RCodeElement",
          function(obj)
      {
          ret = c(obj$attributes, obj$formatSpecific, position = obj$posInParent, id = obj$id, invars = obj$invars, outvars = obj$outvars, content = obj$content)
          as.list(ret)
      })

setMethod("dyndoc_attrs", "DocElement",
          function(obj)
      {
          ret = c(obj$attributes, obj$formatSpecific, position = obj$posInParent, id = obj$id)
          as.list(ret)
      })

setMethod("dyndoc_attrs", "TextElement",
          function(obj)
      {
          ret = c(obj$attributes, obj$formatSpecific, position = obj$posInParent, id = obj$id, content = obj$content)
          as.list(ret)
      })

setMethod("dyndoc_attrs", "ElementInstance",
          function(obj)
      {
          ret = dyndoc_attrs(obj$element)
          ret$outputs = obj$outputs
          ret$doc_position = ret$position
          ret$position = obj$posInParentInst
          as.list(ret)
      })

setMethod("dyndoc_attrs", "ANY", function(obj) list())

    
