setMethod("widgetToIPyNBList", "IWidgetSlider", function(widget)
          {
            list(variable = widget@var, linenum = widget@linenum, type="slider", defaultvalue = widget@default, step = widget@step, min = widget@range[1], max = widget@range[2] )
          } )

setMethod("widgetToIPyNBList", "IWidgetTextbox", function(widget)
          {
            list(variable = widget@var, linenum = widget@linenum, type="textbox", defaultvalue = paste("\"", widget@default, "\"", sep=""))
          })

setMethod("widgetToIPyNBList", "IWidgetIntTextbox", function(widget)
          {
            list(variable = widget@var, linenum = widget@linenum, type="inttextbox", defaultvalue = widget@default)
          })

setMethod("widgetToIPyNBList", "IWidgetNumTextbox", function(widget)
          {
            list(variable = widget@var, linenum = widget@linenum, type="numtextbox", defaultvalue = widget@default)
          })

IPyNBWidgetToWidget = function(l)
  {
    ret = switch(l$type,
      slider = new("IWidgetSlider", step=l$step, range = c(l$min, l$max)),
      textbox = new("IWidgetTextbox"),
      inttextbox = new("IWidgetIntTextbox"),
      numtextbox = new("IWidgetNumTextbox"),
      stop(paste("unrecognized widget type:", l$type))
      )
    ret@var = l$variable
    ret@linenum = as.integer(l$linenum)
    ret@default = l$defaultvalue
    ret
  }
