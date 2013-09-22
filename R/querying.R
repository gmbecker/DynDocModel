
#why are getThread and findPath different functions?? Will getThread eventually do something other than just call findPath?

getThread <- function(doc,
                      start = 1,
                      end = length(doc$elements),
                      branches = NULL,
                      branch_path = "/*/alt[1]",
                      use_abbrev = TRUE,
                      check_valid = TRUE, ...)
{
    if(is.null(branches) && !is.null(branch_path) && nchar(branch_path))
        branches = dyndoc_rpath(doc, branch_path)
    if(is.numeric(start) || is.integer(start))
        start = doc[[start]]
    if(is.numeric(end) || is.integer(end))
        end = doc[[end]]
    findPath(doc, start, end, visit = branches, check_valid = check_valid, ...)
    
}

findPath = function(doc, start, end, visit, stop_on_fail = TRUE, check_valid = TRUE, ...)
{
    curlev = list()
    curel = end
    
    while(!sameElement(start, curel))
    {
#        inst = makeInstance(curel, doKids = FALSE, branchInstr = visit)
        inst = makeInstance(curel,  branchInstr = visit)
        
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
            } else {
                if(stop_on_fail)
                    stop("Unable to determine path between start and end elements")
                else
                    return(NULL)
            }
        }
    }
    
    curlev= c(start, curlev)
    
    if(check_valid)
    {
        terms = sapply(curlev[-length(curlev)], function(x) is_termBranch(x))
        if(any(terms))
        {
            if(stop_on_fail)
                stop("Detected path passes through a terminal branch.")
            else
                return(NULL)
        }
    }

    new("DocThread", children = curlev, parentDoc = doc, ...)
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
    all = -el$posInParent,
    before = if(el$posInParent ==1) numeric() else seq(1, el$posInParent -1),
    after = if(el$posInParent == length(sibs)) numeric() else seq(el$posInParent +1, length(sibs))
    )
    sibs[inds]
}

makeInstance = function(el, branchInstr = list(), doKids = TRUE)
{
    if(is(el, "ElementInstance"))
    {
        if(is_selfOrEl(el, "ContainerElement") && doKids && !length(el$children))
            el$instanceChildren()
        return(el)
    }
    
    ret = NULL
    
    if(is(el, "BranchSetElement"))
    {
 
        for(target in branchInstr)
        {
            found = any(sapply(el$children, sameElement, el2 = target))
            if(found)
                return(makeInstance(target, doKids = doKids))
        }
        ret = makeInstance(el[[1]], doKids = doKids)
        
    } else if(is(el, "MixedTextElement") || (is(el, "ContainerElement") && doKids)) {
        
        ret = new("ElementInstance", element = el, instanceChildren = TRUE)
#        kids = vector("list", length(el$children))
 #       for(k in seq(along = el$children))
  #      {
   #         kids[[k]] = makeInstance(el$children[[k]])
    #    }
     #   ret$children = kids
    } else {
        ret = new("ElementInstance", element = el, instanceChildren = FALSE)
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



checkNodeAttrs = function(node, attrs)
{
    ## Right now this will only check for full equality
    attrs = as.list(attrs)
    all(mapply(function(nm, val) all(node$field(nm) == val),
               nm = names(attrs),
               val = attrs))
}


#this implementation will probably be really slow
getAllThreads = function(doc, start = 1 , end = length(doc$elements) , only_valid=TRUE)
{
#    allbsets = dyndoc_rpath(doc, "//altset",  names_fun = dyndoc_rpath_abbrevs2)
 #   if(!length(allbsets))
  #      return(list(getThread(doc)))
    
   # rootInds = which(sapply(allbsets, firstBranchingSince))
    #rootBSets = allbsets[rootInds]
    #remaining = allbsets[-rootInds]
    
    branchInstr = expandBranches(doc)
    if(!length(branchInstr))
        branchInstr = list(list())
    
    lapply(branchInstr, function(instr) getThread(doc, start, end, branches = instr, check_valid = only_valid, stop_on_fail=FALSE))
}



expandBranches = function(parent, prev = list())
{
    
    altsets = getFirstBranchings(parent)
    #if there aren't any more branchings
   # if(!length(altsets))
    #    return(list())
    
  #  if(length(prev))
         ret = list(prev)
   # else
    #    ret = list()

    for(pt in altsets)
    {
        onestep = list()
        for(br in pt$children)
            onestep = c(onestep,  expandBranches(br, prev = list(br)))
        ret = allCombos(ret, onestep)
     #   tmp =lapply(pt$children, function(br) expandBranches(br, list(br)))
        
      #  ret = addToAll(ret, tmp, both = TRUE)
    }
    ret
}

allCombos = function(lst, add, rev = FALSE)
{
    ret = rep(lst, times = length(add))
    toadd = rep(add, times = rep(length(lst), times = length(add)))
    mapply(function(x,y) if(!rev) c(x,y) else c(y,x) , ret, toadd, SIMPLIFY = FALSE)
}



setGeneric("getFirstBranchings", function(el, found = NULL) standardGeneric("getFirstBranchings"))

setMethod("getFirstBranchings", "ContainerElement",
          function(el, found) {
              .getFirstBranchings(el$children, found)
          })


setMethod("getFirstBranchings", "DynDoc",
          function(el, found) {
              .getFirstBranchings(el$elements, found)
          })

setMethod("getFirstBranchings", "DocElement",
          function(el, found) {
              list()
          })

setMethod("getFirstBranchings", "ElementInstance",
          function(el) {
              .getFirstBranchings(el$children, found)
          })
          

 

.getFirstBranchings = function(kids, found = list())
{
    possible = sapply(kids, function(x) is_selfOrEl(x, "ContainerElement"))
    
    if(!any(possible))
        return(list())

    ret = list()
    fnd = list()
    for(kid in kids[possible])
    {
        if(is_selfOrEl(kid, "BranchSetElement"))
            fnd = c(fnd, kid)
        else
            fnd = getFirstBranchings(kid, found = fnd)
    }
    unlist(c(found, fnd))
}



