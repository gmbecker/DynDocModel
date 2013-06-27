getThread = function(doc, start = 1, end = length(doc$elements))
{
    kidInsts = lapply(doc)

}

findPath = function(doc, start, end)
{
    curlev = list()
    curel = end

    while(!sameElement(start, curel))
    {
        inst = makeInstance(curel, doKids = FALSE)

        sibs = getSiblings(curel, posType = "before")
        if(length(sibs))
        {
            curel = sibs[[length(sibs)]]
            curlev = c(inst, curlev)
        } else {
            if(!is(curel$parent, "DynDoc"))
            {
                curel = curel$parent
                inst$children = curlev
                curlev = list(inst)
            }
            stop("Unable to determine path between start and end elements")
        }
    }

    curlev= c(start, curlev)

    new("DocThread", children = curlev, parentDoc = doc)
}

getSiblings = function(el, posType = c("all", "before", "after"))
    {
        if(!is(el$parent, "ContainerElement") & !is(el$parent, "DynDoc"))
            return(list())

        if(is(el$parent, "ContainerElement"))
            sibs = el$parent$children
        else
            sibs = el$parent$elements

        posType = match.arg(posType, c("all", "before", "after"))
        inds  = switch(posType,
                      all = -1,
                      before = if(el$posInParent ==1) numeric() else seq(1, el$posInParent -1),
                      after = if(el$posInParent == length(sibs)) numeric() else seq(el$posInParent +1, length(sibs))
        )
        sibs[inds]


  }

makeInstance = function(el, branchInstr, doKids = TRUE)
{
    ret = new("ElementInstance", element = el)
    if(is(el, "BranchSetElement") && doKids)
   {
       warning("Branching is not yet fully supported. Selecting 'first' branch to construct thread")
       ret = makeInstance(el[[1]])

   } else if(is(el, "ContainerElement") && doKids) {
        kids = allocVector("list", length(el$children))
        for(k in seq(along = el$children))
        {
            kids[[k]] = makeInstance
        }
        ret$children = kids
    } else {
        ret = new("ElementInstance", element = el)
    }

    ret
}



subquery = function(start, type=character(), attrs=list(), position=integer(), fun = NULL, all.levels = FALSE, parent=FALSE)
{
  if(!is.null(fun) && (length(type) || length(attrs) ))
    warning("filter function (fun) and other attributes (type, attrs) were specified for subquery. Only filter function and position will be applied.")

  #if we have multiple starting points, eg the output of a previous call to subquery, query each one one at a time and combine the results
  if(is(start, "list"))
    {
      ret = unlist(lapply(start, subquery, type=type, attrs = attrs, fun = fun, all.levels = all.levels), recursive=FALSE)
      return(as(ret, "ElementList"))
    }

  if(is(start, "DynDoc"))
    {
      if(!parent)
        els = start$elements
      else
        return(as(list(), "ElementList")) #there are no parents
    }
  else if (is(start, "ContainerElement"))
    if(!parent)
      els = start$children
    else
      {
        if(is(start$parent, "DynDoc") || is.null(start$parent))
          return(as(list(), "ElementList")) #there are no parent elements
        else
          els = as(list(start$parent), "ElementList")
      }
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
  if(all.levels)
    #if we are searching through the whole (sub)tree, apply the same query to all children of start (this is recursive)
    #This search is breadth first NOT depth first
    ret = c(ret, subquery(els, type=type, attrs = attrs, fun=fun, all.levels=TRUE, parent = parent))
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
