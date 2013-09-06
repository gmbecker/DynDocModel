

addCellHTML = function(outdoc, content)
    {
        if(is.character(content))
            content = getNodeSet(htmlParse(content), "//body/*")
        if(!list(content))
            content = list(content)

        div = newXMLNode("div", .children = content, attrs = c(class = "dyndoc_container"))
        node = getNodeSet(outdoc, "//div[@id='dyndoc_main_container']")[[1]]
        addChildren(node, div)
        outdoc
    }

#IPyNB is easy because it is just a list that will be JSONified during the finish call
addCellIPyNB = function(outdoc, content)
    {
        cells = outdoc$worksheets[[1]]$cells
        ncells = length(cells)
        cells[[ncells+1]] = content
        outdoc$worksheets[[1]]$cells = cells
        outdoc
    }

addCellRdb = addCellRmd = addCellTex = function(outdoc, content) stop("not yet implemented")
DefaultAddCells = list(
    html = addCellHTML,
    ipynb = addCellIPyNB,
    rmd = addCellRmd,
    rdb = addCellRdb,
    tex = addCellTex,
    pdf = addCellTex
    )

getDefaultAddCell = function(format)
    {
        ret = DefaultAddCells[[tolower(format)]]

        if(is.null(ret))
            stop(sprintf("No default addCell function found for format %s", format))
        ret
    }
