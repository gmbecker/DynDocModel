#We expect an arbitrary number of element lists, each of which will be transformed into a separate AlternativeImplElement.
makeAltImplSet = function(...)
  {
    listolists = list(...)
    firstel = listolists[[1]][[1]]

    #I think this is implicitly assuming the elements we're removing are contiguous, will they always be? At the very least they currently need to all have the same parent
    parent = firstel$parent
    pos = min(sapply(listolists, function(x) min(sapply(x, function(el) el$posInParent))))
    new_alt_els = as(lapply(listolists,  makeOneAltImpl), "ElementList")
        
    newAltSet = altImplSetElement$new(children = new_alt_els, parent=parent)
    invisible(parent$insertChildren(newAltSet, startPos = pos))

  }

makeOneAltImpl = function(ellist)
  {
    if(length(ellist) ==1 && !is.list(ellist))
      ellist =new("ElementList", ellist)

    
    if(!is(ellist, "ElementList"))
      stop("Only ElementList objects or single DocElement objects can be used to create alternative implementations")

    parent = ellist[[1]]$parent
    if(class(parent) == "uninitializedField")
      parent = NULL
    inds = sort(sapply(ellist, function(el) el$posInParent))
    if(is(parent,"DynDoc"))
      {
        parent$children = parent$children[-inds]
        sapply(seq(along=parent$children), function(i) parent$children[[i]]$posInParent = i)
      }
    else if(is(parent, "ContainerElement"))
      {
        parent$children = parent$children[-inds]
        sapply(seq(along=parent$children), function(i) parent$children[[i]]$posInParent = i)
      }
    newalt = altImplElement$new(parent = parent)
    newalt$insertChildren(ellist, startPos = 1)
       
    #sapply(seq(along=ellist), function(i) ellist[[i]]$posInParent = i)
    newalt

  }


collapseAltImplSet = function(altset)
  {
    parent = altset$parent
    gkids = as(unlist(lapply(altset$children, function(el) el$children), recursive=FALSE), "ElementList")
    pos = altset$posInParent
    parent$insertChildren(gkids, pos)
  }
