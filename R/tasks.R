makeTask = function(ellist)
  {
    if(length(ellist) ==1 && !is.list(ellist))
      ellist =new("ElementList", ellist)

    if(!is(ellist, "ElementList"))
      stop("Only ElementList objects or single DocElement objects can be used to create tasks")

    parent = ellist[[1]]$parent
    newtask = taskElement$new(parent = parent, children=ellist)
    

    inds = sort(sapply(ellist, function(el) el$posInParent))
    parent[[inds[1]]] = newtask
    newtask$posInParent = inds[1]
    sapply(seq(along=ellist), function(i) ellist[[i]]$posInParent = i)
    if(class(parent)=="DynDoc")
      parent$elements = parent$elements[-inds[-1]]
    else
      parent$children = parent$children[-inds[-1]]
    
    parent
  }

collapseTask = function(task)
  {
    parent = task$parent
    kids = task$children
    pos = task$posInParent
    parent$removeChild(task)
    parent$insertChildren(kids, pos)
  }

