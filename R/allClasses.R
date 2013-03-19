setClass("ElementList", contains="list")

setClassUnion("ListOrNull", members = c("list", "NULL"))
dynDoc = setRefClass("DynDoc", fields = list(
                                 elements = "ElementList",
                                 metadata = "ListOrNull",
                                 formatSpecific = "ListOrNull"),
  methods = list(
    addChild = function(newel)
    {
      newpos = length(elements)+1
      newel$posInParent = newpos
      elements[[newpos]] <<- newel
    },
    insertChildren = function(elList, startPos)
    {
      if (is(elList, "DocElement"))
        elList = as(list(elList), "ElementList")
      
      if(startPos > length(elements))
        return(sapply(elList, function(el) .self$addChild(el)))
      afterinds = seq(startPos, length(elements))
      elsafter = elements[afterinds]
      elsbefore = elements[-afterinds]
      elements <<- as(c(elsbefore, elList, elsafter), "ElementList")
      sapply(seq(startPos, length(elements)), function(i) elements[[i]]$posInParent = i)
      
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
                                         metadata = "ListOrNull",
                                         formatSpecific = "ListOrNull",
                                         posInParent = "numeric",
                                         id = "character"))

containerElement = setRefClass("ContainerElement", contains = "DocElement",
  fields = list(children = "ElementList"),
  methods = list(
    addChild = function(newel)
    {
      newpos = length(children) + 1
      newel$posInParent = newpos
      children[[newpos]] <<- newel
    })
  )
taskElement = setRefClass("TaskElement", contains = "ContainerElement")

codeElement = setRefClass("CodeElement", contains = "DocElement",
  fields = list(content = "character",
    outputs = "ElementList"))

rCodeElement = setRefClass("RCodeElement", contains = "CodeElement")

pyCodeElement = setRefClass("PyCodeElement", contains = "CodeElement")

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

altImplElement = setRefClass("AltImplElement", contains = "BranchElement")

altMethodElement = setRefClass("AltMethodElement", contains = "BranchElement")

altQuestElement = setRefClass("AltQuestElement", contains = "BranchElement")

sectElement = setRefClass("SectionElement", contains = "BranchElement")
