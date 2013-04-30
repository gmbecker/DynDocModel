makeInteractive = function(node, vars, defaults = rep(NA, times=length(vars)), control.types = getControlTypes(defaults, addl.info), addl.info, linenums = NA)
  {
    controls = makeInteractiveWidgets(node, vars, defaults, control.types, addl.info, linenums)
    
    newclass = switch(class(node),
      RCodeElement = "IntRCodeElement",
      PyCodeElement = "IntPyCodeElement",
      default = "IntCodeElement")
    
    newnode = as(node, newclass)
    newnode$widgets = controls
    node$parent[[node$posInParent]] = newnode
    return(newnode)
  }

makeInteractiveWidgets = function(node, vars, defaults, control.types = getControlTypes(defaults, addl.info), addl.info, linenums = NA)
  {
    if(length(vars) > 1)
      {
        #the list() call is to try to get it to recycle properly, we'll see if it works
        ret = as(list(unlist(mapply(makeInteractiveWidget, list(node), vars, defaults, control.types, addl.info, linenums),recursive=TRUE)), "WidgetsList")
        return(ret)
      }
    if(is.na(linenums))
      linenums = getFirstAssignLN(var = vars, node = node)
    ret = new(control.types, var = vars, widget = control.types, linenum = linenums, default = defaults, additional.info = addl.info)
    #add extra bits
    if(control.types == "IWidgetSlider")
      {
        if(!("range" %in% names(addl.info)))
          stop("Attempted to create slider widget without specifying range in addl.info")
        ret@range = addl.info$range
        ret@step = if("step" %in% names(addl.info)) addl.info$step else 1
      }
    as(list(ret), "WidgetsList")
  }

getControlType = function(defaults, add.info)
{
  if(length(defaults) > 1)
    return(mapply(getControlTypes, defaults, add.info))
  if(is.na(defaults))
    return("IWidget")
 
  switch(class(defaults),
         integer = if("range" %in% names(add.info)) "IWidgetSlider" else "IWidgetIntTextbox",
         numeric = if("range" %in% names(add.info)) "IWidgetSlider" else "IWidgetNumTextbox",
         character = "IWidgetTextbox",
         default = "IWidget")
}
