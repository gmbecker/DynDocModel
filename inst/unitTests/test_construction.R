
test_construction = function()
  {
    textEl =  textElement$new(content = "hi there!")
    doc = dynDoc$new(elements = textEl)
    checkTrue(class(doc[[1]]$parent) == "DynDoc")

    morecells = as(lapply(1:7, function(i) textElement$new(content=as.character(i))), "ElementList")

    checkTrue(length(doc$elements) == 1)
    doc$insertChildren(morecells, 2)
    checkTrue(length(doc$elements) == 8)
    checkTrue(all(sapply(doc$elements, function(el) class(el$parent) == "DynDoc")))
    checkTrue(all(sapply(doc$elements, function(e) e$posInParent) == seq(along = doc$elements)))
  }

test_task = function()
  {
    textEl =  textElement$new(content = "hi there!")
    doc = dynDoc$new(elements = textEl)
    morecells = as(lapply(1:7, function(i) textElement$new(content=as.character(i))), "ElementList")
    doc$insertChildren(morecells, 2)

    makeTask(doc[1:3])
    checkTrue(class(doc[[1]]) == "TaskElement")
    checkTrue(length(doc$elements) == 6)
    #make sure parents are getting modified correctly
    checkTrue(class(doc[[1]]$parent) == "DynDoc")
    checkTrue(all(sapply(doc[[1]]$children, function(el) class(el$parent) == "TaskElement")))
    #make sure posInParent values are getting updated properly
    checkTrue(all(sapply(doc$elements, function(el) el$posInParent) == seq(along = doc$elements)))
    checkTrue(all(sapply(doc[[1]]$children, function(el) el$posInParent) == seq(along = doc[[1]]$children)))
  }

test_altImplSet = function()
  {

    textEl =  textElement$new(content = "hi there!")
    doc = dynDoc$new(elements = textEl)
    morecells = as(lapply(1:7, function(i) textElement$new(content=as.character(i))), "ElementList")
    doc$insertChildren(morecells, 2)

    makeAltImplSet(doc[2:3], doc[4:6] )
    altset = doc[[2]]
    checkTrue(class(altset) == "AltImplSetElement")
    checkTrue(length(doc$elements) == 4)
    checkTrue(length(altset$children) == 2)
    checkTrue(all(sapply(altset$children, function(el) length(el$children)) == c(2, 3)))
    #make sure parents are getting modified correctly
    checkTrue(class(altset$parent) == "DynDoc")
    checkTrue(all(sapply(altset$children, function(el) class(el$parent) == "AltImplSetElement")))
    #make sure posInParent values are getting updated properly
    checkTrue(all(sapply(doc$elements, function(el) el$posInParent) == seq(along = doc$elements)))
    checkTrue(all(sapply(altset$children, function(el) el$posInParent) == seq(along = altset$children)))
    checkTrue(all(sapply(altset$children, function(el) all(sapply(el$children, function(kid) kid$posInParent) == seq(along=el$children)))))
  }
