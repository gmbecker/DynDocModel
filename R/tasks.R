makeTask = function(ellist)
  {
    if(length(ellist) ==1 && !is.list(ellist))
      ellist =new("ElementList", ellist)

    if(!is(ellist, "ElementList"))
      stop("Only ElementList objects or single DocElement objects can be used to create tasks")

    parent = ellist[[1]]$parent

    inds = sort(sapply(ellist, function(el) el$posInParent))
    if(class(parent)=="DynDoc")
      parent$elements = parent$elements[-inds]
    else
      parent$children = parent$children[-inds]

    newtask = taskElement$new(parent = parent, children=ellist)
    parent$insertChildren(newtask, inds[1])
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

