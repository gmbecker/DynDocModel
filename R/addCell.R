addCellMD = function(outdoc, content)
{
    c(outdoc, content)

}

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

addCellHTML = addCellMD
#IPyNB is easy because it is just a list that will be JSONified during the finish call
addCellIPyNB = function(outdoc, content)
    {
        cells = outdoc$worksheets[[1]]$cells
        ncells = length(cells)
        cells[[ncells+1]] = content
        outdoc$worksheets[[1]]$cells = cells
        outdoc
    }

addCellTex = addCellMD

addCellRdb = function(outdoc, content)
{
    if(!is.list(content))
        content = list(content)
    addChildren(xmlRoot(outdoc), kids = content)
}


addCellRmd = function(outdoc, content) stop("not yet implemented")



DefaultAddCells <- list(
    html = addCellHTML,
    md = addCellMD,
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
        {
            warning(sprintf("No default addCell function found for format %s", format))
            ret = function(outdoc, content) c(outdoc, content)
        }
        ret
    }
