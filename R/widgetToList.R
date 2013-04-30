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
