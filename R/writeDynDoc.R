#XXX Need to add defaults to lists so that people can specify, e.g. a custom formatter for data.frame objects and have the default formatter used for the rest. Same goes for cell types in cellRenderers

#addCell accepts an *already rendered* cell and places it in the output document, so no need for different behavior or dispatch at the cell type level for it, only at the format level

DefaultRenderers <- list(
    rmd = renderCellRmd,
    ipynb = renderCellIPyNB,
    html = renderCellMD, #to get HTML we make markdown and call markdownToHTML in the finish method
    md = renderCellMD,
    rdb = renderCellRdb,
    pdf = renderCellTex,
    tex = renderCellTex
)

getDefaultRenderer = function(format)
{
    rend = NULL
    if(!is.null(format))
        rend = DefaultRenderers[[tolower(format)]]
    
    if(is.null(rend))
        stop(sprintf("no default cell renderer found for format %s", tolower(format)))
    rend
}


writeDynDoc = function(doc,
    file,
    output.format = {if(!is.null(file)) tolower(gsub(".*\\.(.*)$", "\\1", file)) else NULL},
#    formatters = getDefaultFormatter(outFormat),
    formatters = formatObject,
    cell.renderers = getDefaultRenderer(output.format),
    init.output = getDefaultInit(output.format),
    finish.output = getDefaultFinish(output.format),
    addCell = getDefaultAddCell(output.format),
    converters = list(),                    
    ...)
{
    if(any(sapply(doc$children, function(x) is(x, "DecisionElement"))))
        warning("rendering of branching documents is probably not properly implemented yet")

    state = new.env()
    state$plots = 1
    state$outdir = dirname(file)
    state$basePlotName = paste(gsub("(.*)\\..*$", "\\1", basename(file)), "plot", sep="_")
    out = init.output(file, doc)
 
    foundRenderers = list()
    for (el in doc$children)
    {
        if(is(el, "ElementInstance") && length(el$formatters))
        {    
            tmpformatters = c(el$formatters, if(is.function( formatters)) list("ALL" = formatters) else formatters)
            tmpformatters = tmpformatters[!duplicated(names(tmpformatters))]
        } else {
            tmpformatters = formatters
        }
        
        if(is.function(cell.renderers))
            rcell = cell.renderers(el, tmpformatters, state = state, converters = converters)
        else
        {
            if(class(el) %in% names(foundRenderers))
                rcell = foundRenderers[[class(el)]](el, tmpformatters, state = state, converters = converters)
            else
            {
                meth = doListDispatch(class(el), cell.renderers)
                rcell = meth(el, tmpformatters, state = state, converters = converters)
                foundRenderers[[class(el)]] = meth
            }
            
        }
        if(is(rcell, "FormattedOutputList"))
        {
            for(ocell in rcell)
                out = addCell(out, ocell)
        } else {
            out = addCell(out, rcell)
        }
    }
    finish.output(out, file, doc)
}


