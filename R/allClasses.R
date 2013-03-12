setClass("ElementList", contains = "list")
setClassUnion("ListOrNull", members = c("list", "NULL"))
dynDoc = setRefClass("DynDoc", fields = list(
                                 elements = "ElementList",
                                 metadata = "ListOrNull",
                                 formatSpecific = "ListOrNull"),
  methods = list(
    addChild = function(newel)
    {
      elements[length(elements)+1] <<- newel
    })
  )

docElement = setRefClass("DocElement", fields = list(
                                         parent = "ANY",
                                         metadata = "ListOrNull",
                                         formatSpecific = "ListOrNull"))

containerElement = setRefClass("ContainerElement", contains = "DocElement",
  fields = list(children = "ElementList"),
  methods = list(
    addChild = function(newel)
    {
      children[length(children) + 1] <<- newel
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
