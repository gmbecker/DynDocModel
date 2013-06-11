setClass("ElementList", contains="list")
setClassUnion("ListOrNull", members = c("list", "NULL"))

setClass("WidgetsList", contains="list")
setClass("IWidget", representation(var = "character",
 #I think we want to specify widget by class, but I'll leave this here for custom widgets?
                                   widget = "character",
                                   linenum = "integer",
                                   default = "ANY",
                                   additional.info = "list"))

setClass("IWidgetSlider", representation(range = "numeric",
                                         step = "numeric"),
         contains = "IWidget")


setClass("IWidgetTextbox", contains = "IWidget")
setClass("IWidgetIntTextbox", contains = "IWidgetTextbox")
setClass("IWidgetNumTextbox", contains = "IWidgetTextbox")

dynDoc = setRefClass("DynDoc", fields = list(
                                 .envir = "environment",
                                 envir = function(value)
                                 {
                                   if(missing(value))
                                     .envir
                                   else
                                     .envir <<- value
                                 },
                                 .elements = "ElementList",
                                 elements = function(value)
                                 {
                                   if(missing(value))
                                     .elements
                                   else
                                     {
                                       if(is(value, "DocElement"))
                                         value = as(list(value), "ElementList")
                                       value = as(sapply(seq(along = value), function(i)
                                              {
                                                el = value[[i]]
                                                el$parent = .self
                                                el$posInParent = i
                                                el
                                              }), "ElementList")
                                       .elements <<- value
                                       
                                     }
                                 },
                                 metadata = "ListOrNull",
                                 formatSpecific = "ListOrNull"),
  methods = list(
    addChild = function(newel)
    {
      newpos = length(elements)+1
      newel$posInParent = newpos
      newel$parent = .self
      elements[[newpos]] <<- newel
      newel
    },
    insertChildren = function(elList, startPos)
    {
      if (is(elList, "DocElement"))
        elList = as(list(elList), "ElementList")

      if(!is(elList, "ElementList"))
        stop("elList does not seem to be an ElementList. Unable to insert children")
      if(startPos > length(elements))
        return(sapply(elList, function(el) .self$addChild(el)))
      afterinds = seq(startPos, length(elements))
      elsafter = elements[afterinds]
      elsbefore = elements[-afterinds]
      .self$elements = as(c(elsbefore, elList, elsafter), "ElementList")
      #parent/child stuff taken care off in field assignment
     
    },
    removeChild = function(oldel)
    {
      elements <<- elements[-oldel$posInParent]
      oldel$parent = NULL
      oldel$posInParent = 0
    })
  )

docElement = setRefClass("DocElement", fields = list(
                                         parent = "ANY",
                                         #metadata = "ListOrNull", #attributes makes more sense, so changing to that
                                         attributes = "ListOrNull",
                                         formatSpecific = "ListOrNull",
                                         posInParent = "numeric",
                                         id = "character"))

containerElement = setRefClass("ContainerElement", contains = "DocElement",
  fields = list(
    .envir = "environment",
    envir = function(value)
    {
      if(missing(value))
        .envir
      else
        .envir <<- value
    },
    .children = "ElementList",
    children = function(value)
    {
      if(missing(value))
        .children
      else
        {
          if(is(value, "DocElement"))
            value = as(list(value), "ElementList")
          value = as(sapply(seq(along = value), function(i)
                                              {
                                                el = value[[i]]
                                                el$parent = .self
                                                el$posInParent = i
                                                el
                                              }), "ElementList")

          .children <<- value
          .self$resetInOutVars()
        }
    }
    ),
  methods = list(
    resetInOutVars = function()
    {
      listin = lapply(.self$children, function(el)
        {
          if(!(is(el, "CodeElement")||is(el, "ContainerElement")))
            return(NULL)
          el$resetInOutVars()
          el$invars
        })
      listout = lapply(.self$children, function(el)
        {
          if(!(is(el, "CodeElement")||is(el, "ContainerElement")))
            return(NULL)
          el$outvars
        })

      retin = character()
      retout = character()
      for(i in seq(along=listin))
        {
          tmpin = listin[[i]]
          if(!is.null(tmpin))
            {
              retin = c(retin, tmpin[! (tmpin %in% retout) ] )
              retout = c(retout, listout[[i]])
            }
        }
                
      .self$invars = retin
      .self$outvars = retout

    },
    addChild = function(newel)
    {
      newpos = length(children) + 1
      newel$posInParent = newpos
      newel$parent = .self
      children[[newpos]] <<- newel
      newel
    },
    insertChildren = function(elList, startPos)
    {
      if (is(elList, "DocElement"))
        elList = as(list(elList), "ElementList")

      if(!is(elList, "ElementList"))
        stop("elList does not seem to be an ElementList. Unable to insert children")
      if(startPos > length(children))
        return(sapply(elList, function(el) .self$addChild(el)))
      afterinds = seq(startPos, length(children))
      elsafter = children[afterinds]
      elsbefore = children[-afterinds]
      .self$children = as(c(elsbefore, elList, elsafter), "ElementList")
      #parent child stuff is now taken care of in activeBinding methods on the fields themselves.
      
    })
  )
taskElement = setRefClass("TaskElement", contains = "ContainerElement")

codeElement = setRefClass("CodeElement", contains = "DocElement",
  fields = list(
    .envir = "environment",
    envir = function(value)
    {
      if(missing(value))
        .envir
      else
        .envir <<- value
    },
    .hash = "character",
    .invars = "character",
    invars = function(value)
    {
      if(missing(value))
        .invars
      else
        .invars <<- value
    },
    .outvars = "character",
    outvars = function(value)
    {
      if(missing(value))
        .outvars
      else
        .outvars <<- value
    },
    content = "character",
    outputs = "ElementList"))

rCodeElement = setRefClass("RCodeElement", contains = "CodeElement")
pyCodeElement = setRefClass("PyCodeElement", contains = "CodeElement",
  methods = list(
    resetInOutVars = function() NULL #currently we don't know how to do this for python
    ))

intCodeElement = setRefClass("IntCodeElement", contains = "CodeElement",
  fields = list(widgets = function(val)
    {
      if(missing(val))
        .widgets
      else
        {
          if(is(val, "IWidget"))
            val = list(val)
          .widgets <<- val
          }
      },
    .widgets = "WidgetsList"))

intRCodeElement = setRefClass("IntRCodeElement", contains = c("IntCodeElement", "RCodeElement"))

intPyCodeElement = setRefClass("IntPyCodeElement", contains = c("IntCodeElement", "PyCodeElement"))

outputElement = setRefClass("OutputElement", contains = "DocElement",
  fields = list(
    codeElement = "CodeElement",
    format = "ANY",
    content="ANY"
    ))

textElement = setRefClass("TextElement", contains="DocElement",
  fields = list(content="ANY"))

mdElement = setRefClass("MDTextElement", contains="TextElement")

dbElement = setRefClass("DbTextElement", contains="TextElement")

branchElement = setRefClass("BranchElement", contains = "ContainerElement")

branchSetElement = setRefClass("BranchSetElement", contains = "ContainerElement")
altImplElement = setRefClass("AltImplElement", contains = "BranchElement")
altImplSetElement = setRefClass("AltImplSetElement", contains = "BranchSetElement")

altMethodElement = setRefClass("AltMethodElement", contains = "BranchElement")
altMethodSetElement = setRefClass("AltMethodSetElement", contains = "BranchSetElement")
altQuestElement = setRefClass("AltQuestElement", contains = "BranchElement")
altQuestSetElement = setRefClass("AltQuestSetElement", contains = "BranchSetElement")

sectElement = setRefClass("SectionElement", contains = "ContainerElement",
  fields = list(
    .title = "character",
    title = function(value)
    {
      if(missing(value))
        .title
      else
        .title <<- value
      }
    ))
 
headerSectElement = setRefClass("HeaderSectElement", contains = "SectionElement")
