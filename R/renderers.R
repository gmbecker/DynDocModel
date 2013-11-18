if(FALSE)
    {
DefaultRenderers = list(
    rmd = renderCellRmd,
    ipynb = renderCellIPyNB,
    html = renderCellHTML,
    rdb = renderCellRdb,
    pdf = renderCellTex,
    tex = renderCellTex
)

getDefaultRenderer = function(format)
    {
        if(!is.null(format))
            rend = DefaultRenderers[tolower(format)]

        if(is.null(rend))
            stop(sprintf("no default cell renderer found for format %s", tolower(format)))
        rend
    }

writeRmd = function(doc, file, formatters, cellRenderers = getDefaultRenderer("rmd"),  ...)
  {
    stop("not yet supported")

  }

writeHTML = function(doc, file, formatters, cellRenderers = getDefaultRenderer("html"),  initOuput = makeHeaderHTML, finishOutput = makeFooterHTML, addCell = addCellHTML, ...)
    {
        if(any(sapply(doc$children, function(x) is(x, "DecisionElement"))))
            warning("rendering of branching documents is probably not properly implemented yet")

        foundRenderers = list()
        out = initOutfile(file, doc)
        for (el in doc$children)
            {
                if(is.function(cellRenderers))
                    rcell = cellRenderers(el)
                else
                    {

                    }
                addToFile(out, rcell)
            }
        finishOutput(out, file, doc)
    }

writePDF = function(doc, file, formatters, cellRenderers = getDefaultRenderer("pdf"),  ...)
    {
        stop("not yet supported")
    }


writeIPyNB = function(doc, file, formatters, cellRenderers = getDefaultRenderer("ipynb"),  ...)
    {

        stop("not yet supported")
    }

writeRdb = function(doc, file, formatters, cellRenderers = getDefaultRenderer("rdb"),  ...)
    {
        stop("not yet supported")
    }
}
