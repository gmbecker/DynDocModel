subquery = function(start, type=character(), attrs=list(), position=integer(), fun = NULL, all.descendents = FALSE)
{
  if(!is.null(fun) && (length(type) || length(attrs) ))
    warning("filter function (fun) and other attributes (type, attrs) were specified for subquery. Only filter function and position will be applied.")
  
  #if we have multiple starting points, eg the output of a previous call to subquery, query each one one at a time and combine the results
  if(is(start, "list"))
    {
      ret = unlist(lapply(start, subquery, type=type, attrs = attrs, fun = fun, all.descendents = all.descendents), recursive=FALSE)
      return(as(ret, "ElementList"))
    }
  
  if(is(start, "DynDoc"))
    els = start$elements
  else if (is(start, "ContainerElement"))
    els = start$children
  else
    return(as(list(), "ElementList"))# we are at a terminal node, no children to check.
  
   ret = els ##initialize to the full list, then pare down
  if(!is.null(fun))
    ret = ret[sapply(els, fun)]
  else
    {
      if(length(type))
        {
          matches.type = sapply(ret, function(el) any(sapply(doAbbrevType(type), function(cl) is(el, cl))))
        ret = ret[matches.type]
        }
      if (length(attrs))
        {
          matches.attr = sapply(ret, checkNodeAttrs, attrs = attrs)
          ret = ret[matches.attr]
        }
      #This represents position in the result set, ie I want the third text element, NOT posInParent

    }
  if(all.descendents)
    #if we are searching through the whole (sub)tree, apply the same query to all children of start (this is recursive)
    #This search is breadth first NOT depth first
    ret = c(ret, subquery(els, type=type, attrs = attrs, fun=fun, all.descendents=TRUE))
  if(length(position))
    {
      matches.pos = position[position < length(ret)]
      ret = ret[matches.pos]
    }
  
  return(as(ret, "ElementList"))
}


doAbbrevType = function(types)
  {
    sapply(types, function(ty)
           {
             switch(ty,
                    code = "CodeElement",
                    rcode = "RCodeElement",
                    pycode = "PyCodeElement",
                    text = "TextElement",
                    markdown = "MDTextElement",
                    md = "MDTextElement",
                    docbook = "DbTextElement",
                    db = "DbTextElement",
                    task = "TaskElement",
                    output = "OutputElement",
                    altimpls = "AltImplSetElement",
                    altimpl = "AltImplElement",
                    altmeths = "AltMethodSetElement",
                    altmeth = "AltMethodElement",
                    sect = "SectionElement",
                    branch = "BranchElement",
                    branchset = "BranchSetElement",
                    any = "DocElement", #any should match all nodes
                    "*" = "DocElement",
                    ty #by default assume node type was not abbreviated
                    )
           })
  }
           

checkNodeAttrs = function(node, attrs)
  {
    ## Right now this will only check for full equality
    attrs = as.list(attrs)
    all(mapply(function(nm, val) all(node$field(nm) == val),
               nm = names(attrs),
               val = attrs))
  }
