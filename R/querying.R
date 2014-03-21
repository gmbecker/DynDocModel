
#why are getThread and findPath different functions?? Will getThread eventually do something other than just call findPath?

getThread <- function(doc,
                      start = 1,
                      end = length(doc$children),
                      branches = NULL,
                      branch_path = NULL, #"/*/alt[1]",
                      use_abbrev = TRUE,
                      check_valid = TRUE,
                      cache = doc$cacheEngine,
                      detail_level =1, 
                      ...)
{

    if(is.null(branches) && !is.null(branch_path) && nchar(branch_path))
        branches = dyndoc_rpath(doc, branch_path)
    if(is.numeric(start) || is.integer(start))
        start = doc[[start]]
    if(is.numeric(end) || is.integer(end))
        end = doc[[end]]
    findPath(doc, start, end, visit = branches, check_valid = check_valid, cacheEngine = cache, detail_level = detail_level, ...)
    
}

findPath = function(doc, start, end, visit, stop_on_fail = TRUE, check_valid = TRUE, cacheEngine = doc$cacheEngine, detail_level = 1, decisions_only = FALSE, ...)
{
    curlev = list()
    curel = end
    # we need to handle the case where the document only has one top level element. It could be a single piece of content, but could also be a task/decision.
    if(sameElement(start, end))
    {
        if(detailLevel(end) <= detail_level)
            curlev = list(makeInstance(end, branchInstr = visit, doKids = TRUE, cacheEngine = cacheEngine))
    } else {
        
        while(!sameElement(start, curel))
        {
            #makeInstance is expensive, we want to avoid it when we just want the decision elements
            if(!decisions_only)
                inst = makeInstance(curel,  branchInstr = visit, doKids = TRUE, cacheEngine = cacheEngine)
            else if(is(curel, "DecisionElement"))
            {
                
                inst = getBranchTarget(curel, branchInstr = visit)$target
            } else {
                inst = NULL
            }

            toAdd = (!decisions_only || is(curel, "DecisionElement")) && (detailLevel(curel) <= detail_level)
               
            sibs = getSiblings(curel, posType = "before")
            if(length(sibs))
            {
                curel = sibs[[length(sibs)]]
                if(toAdd)
                    curlev = c(inst, curlev)
               
            } else {
                if(!is(curel$parent, "DynDoc"))
                {
                    curel = curel$parent
                    inst$children = curlev
                    if(toAdd)
                        curlev = list(inst)
                    else
                        curlev = list()
                } else {
                    if(stop_on_fail)
                        stop("Unable to determine path between start and end elements")
                    else
                        return(NULL)
                }
            }
        }
        #make sure the thread visits the specified start element
        if(!is_selfOrEl(start, "DecisionElement") && !decisions_only)
        {
            startInst = makeInstance(start, visit, doKids = TRUE)
            curlev = c(startInst, curlev)
        }
    }
    
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
    if(!decisions_only)
        new("DocThread", children = curlev, parentDoc = doc, cacheEngine = cacheEngine, ...)
    else
        curlev
}

getSiblings = function(el, posType = c("all", "before", "after"))
{
    if(!is(el$parent, "ContainerElement") & !is(el$parent, "DynDoc"))
        return(list())
    
    if(is(el$parent, "ContainerElement"))
        sibs = el$parent$children
    else
        sibs = el$parent$children
    
    posType = match.arg(posType, c("all", "before", "after"))
    inds  = switch(posType,
    all = -el$posInParent,
    before = if(el$posInParent ==1) numeric() else seq(1, el$posInParent -1),
    after = if(el$posInParent == length(sibs)) numeric() else seq(el$posInParent +1, length(sibs))
    )
    sibs[inds]
}

makeInstance = function(el, branchInstr = list(), doKids = TRUE, cacheEngine = el$cacheEngine, ...)
{
    if(is(el, "ElementInstance"))
    {
        if(is_selfOrEl(el, "ContainerElement") && doKids && !length(el$children))
            el$instanceChildren()
        return(el)
    }
    
    ret = NULL
    
    if(is(el, "DecisionElement"))
    {
        targ = getBranchTarget(el, branchInstr)
        ret = makeInstance(targ$target, doKids = TRUE, branchInstr = branchInstr[targ$keepInstr], cacheEngine = cacheEngine, ...)
        
    } else if (is(el, "ContainerElement")) {
        kids = sapply(el$children, makeInstance, doKids = FALSE, branchInstr = branchInstr, cacheEngine = cacheEngine, ...)
        ret = new("ElementInstance", doChildren=FALSE, cachingEngine = cachingEnging, element = el, children = kids)
    { #not a decision element
        ret = new("ElementInstance", element = el, doChildren = FALSE, cacheEngine = cacheEngine, branchInstr = branchInstr, ...)
    }
    
    ret
}


#accepts decision element and a list of branches the full thread will visit
#returns the correct alternative element in $target and the indexes of the remaining (unused) branch instructions in $instrKeep
getBranchTarget = function(dec, branchInstr)
{
        if(is(branchInstr, "AltElement"))
            branchInstr = list(branchInstr)

        indexes = seq(along = branchInstr)
        for(i in indexes)
        {
            target = branchInstr[[i]]
            found = sapply(dec$children, sameElement, el2 = target)
            if(any(found))
                return(list(target = target, instrKeep = indexes[-i]))
        }
        warning("No branch selection instructions for this set of branches. Selecting first non-terminal branch.")
        nonterms = which(sapply(dec$children, function(x) !is_termBranch(x) ) )
        if(!length(nonterms))
        {
            warning("No non-terminal branches found to select as default branch, using first branch.")
            nonterms=1
        }
        list(target = dec[[ nonterms[1] ]], instrKeep = indexes)
}



checkNodeAttrs = function(node, attrs)
{
    ## Right now this will only check for full equality
    attrs = as.list(attrs)
    all(mapply(function(nm, val) all(node$field(nm) == val),
               nm = names(attrs),
               val = attrs))
}





expandBranches = function(parent, prev = list(), detail_level = 1, start = NULL, end = NULL)
{
    
    altsets = getFirstBranchings(parent, detail_level = detail_level, start = start, end = end)
         ret = list(prev)

    for(pt in altsets)
    {
        onestep = list()
        for(br in pt$children)
            onestep = c(onestep,  expandBranches(br, prev = list(br), detail_level = detail_level, start = NULL, end = NULL))
        ret = allCombos(ret, onestep)
    }
    ret
}



getFirstBranchings = function(el, found = NULL, start = NULL, end = NULL, detail_level = 1)
{
    kids = list()
    if(detailLevel(el)  > detail_level)
        return(found)
    
    if(is(el, "DynDoc"))
        kids = el$children
    else if (is(el, "ContainerElement") || is(el, "ElementInstance"))
        kids = el$children

    if(length(kids) && (!is.null(start) || !is.null(end)))
    {
        if(is.null(start))
           start = 1
        if(is.null(end))
            end = length(kids)
        kids = kids[start:end]
    }
    
    .getFirstBranchings(kids, found, detail_level = detail_level)
}

.getFirstBranchings = function(kids, found = list(), detail_level = 1)
{
    possible = sapply(kids, function(x) is_selfOrEl(x, "ContainerElement") && detailLevel(x) <= detail_level)
    
    if(!any(possible))
        return(found)

    ret = list()
    fnd = list()
    for(kid in kids[possible])
    {
        if(is_selfOrEl(kid, "DecisionElement"))
            found = c(found, kid)
        else
            found = getFirstBranchings(kid, found = found, detail_level = detail_level)
    }
                                        #    unlist(c(found, fnd))
    unlist(found)
}




allCombos = function(lst, add, rev = FALSE)
{
    ret = rep(lst, times = length(add))
    toadd = rep(add, times = rep(length(lst), times = length(add)))
    mapply(function(x,y) if(!rev) c(x,y) else c(y,x) , ret, toadd, SIMPLIFY = FALSE)
}

getAllThreads = function(doc, start = 1, end = length(doc$children), detail_level= 1, branches = NULL, branch_path = NULL, use_abbrev = TRUE, check_valid = TRUE, cache = doc$cacheEngine, ...)
{
   if(is.null(branches) && !is.null(branch_path) && nchar(branch_path))
        branches = dyndoc_rpath(doc, branch_path)
    if(is.numeric(start) || is.integer(start))
        start = doc[[start]]
    if(is.numeric(end) || is.integer(end))
        end = doc[[end]]
 
    freedecs = getFreeDecs(doc, start = start, end = end, detail_level = detail_level, branches = branches, cache = cache, check_valid = check_valid)

   instrSets = list(branches)
   for(dec in freedecs)
   {

       allbrs = unlist(lapply(dec$children, function(x) expandBranches(x, prev = list(x))), recursive = FALSE)
       instrSets = allCombos(instrSets, allbrs)
   }

   threads = lapply(instrSets, function(instr) getThread(doc, start, end, branches = instr, detail_level = detail_level, cache = cache, check_valid = check_valid, use_abbrev = use_abbrev))
   as(threads, "ThreadList")
}
    

getFreeDecs = function(doc, start, end, detail_level, branches = NULL, cache, check_valid, ...)
{
    
    alldecs = findPath(doc, start, end, visit = branches, check_valid = check_valid, cacheEngine = cache, detail_level = detail_level, decisions_only = TRUE, ...)

    #the above returns the branches currently. Annoyiinng
    alldecs = sapply(alldecs, function(x) x$parent)

    #decisions which are direct ancestors of the specified end element are not "free", because we have to choose a specific set o alternatives to arrive at end.
    #identify and remove decisions which are fixed due to branch or endpoint instructions
    branches = c(branches, end)
    for( i in seq(along = branches))
    {
        alldecs = remAncestors(alldecs, branches[[i]])
    }

    #nesting is imperfectly handled here, so we remove the ones we catch and then catch them all later
    for(j in seq(along=alldecs))
    {
        alldecs = remAncestors(alldecs, alldecs[[j]])
    }

    
    alldecs

}


#accepts a list of elements (decList) and a single element
#returns the list with entries that were ancestors of element removed
remAncestors = function(decList, element)
{
    retList = decList
    curel = element$parent
    while(!is(curel, "DynDoc"))
    {
        found = sapply(retList, function(x) sameElement(x, curel))
        retList = retList[!found]
        curel = curel$parent
    }
    retList
}
        






containerToKids = function(inst)
{
    kids = list()
    for(el in inst$children)
    {
        if(!length(el$children))
            kids = c(kids, el)
        else
            kids = c(kids, containerToKids(el))
    }
    unlist(kids)
}

collapseThread = function(thread)
{
    kids = containerToKids(thread)
    thread$children = kids
    thread
}
