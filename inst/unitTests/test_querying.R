test_subquery1 = function()
  {
    textEl =  textElement$new(content = "hi there!")
    doc = dynDoc$new(elements = textEl)
    checkTrue(length(subquery(doc, type="text")) == 1)
    
    morecells = as(lapply(1:7, function(i) textElement$new(content=as.character(i))), "ElementList")
    doc$insertChildren(morecells, 2)
    checkTrue(length(subquery(doc, type="text")) == 8)

    checkTrue(length(subquery(doc, type="text", position=3:5)) == 3)
  }

test_subquery_alldesc = function()
  {
    textEl =  textElement$new(content = "hi there!")
    doc = dynDoc$new(elements = textEl)
    morecells = as(lapply(1:7, function(i) textElement$new(content=as.character(i))), "ElementList")
    doc$insertChildren(morecells, 2)

    makeAltImplSet(doc[2:3], doc[4:6] )
    altset = doc[[2]]

    checkTrue(length(subquery(doc, type="text")) == 3)
    checkTrue(length(subquery(doc, type="text", all.descendents=TRUE)) == 8)
}

test_subquery_fun = function()
  {
    textEl =  textElement$new(content = "hi there!")
    doc = dynDoc$new(elements = textEl)
    morecells = as(lapply(1:7, function(i) textElement$new(content=as.character(i))), "ElementList")
    doc$insertChildren(morecells, 2)

    checkTrue(length(subquery(doc, fun = function(x) is(x, "TextElement") && x$content %in% as.character(1:7))) == 7)
    
    makeAltImplSet(doc[2:3], doc[4:6] )

    checkTrue(length(subquery(doc, fun = function(x) is(x, "TextElement") && x$content %in% as.character(1:7))) == 2)
    checkTrue(length(subquery(doc, fun = function(x) is(x, "TextElement") && x$content %in% as.character(1:7), all.descendents = TRUE)) == 7)
    
}
