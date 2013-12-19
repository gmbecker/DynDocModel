finishIPyNB = function(output, file, doc)
{
    cat(file = file, toJSON(output))
    if(inherits(file, "connection"))
        close(file)
}

finishMD = function(output, file, doc)
{
    cat(file = file, output)
    if(inherits(file, "connection"))
        close(file)
}

finishHTML = function(output, file, doc)
{
    #lazy evaluation name conflict in markdownToHTML...
    out = output
    #The rendering was all to MD, so just call markdownToHTML on it...
    markdownToHTML(output = file, text = out)
}

finishTex = function(output, file, doc)
{
    cat(file = file, pandoc_convert(output, out_format="latex", extra_args="--standalone --highlight-style pygments"))
    if(inherits(file, "connection"))
        close(file)
}

finishPDF = function(output, file, doc)
{
    
    filstuff = gsub("\\.pdf", "", file)
    texfile = paste0(filstuff, ".tex")
    finishTex(output, texfile, doc)
    tools::texi2pdf(texfile, clean=TRUE) 

}

finishRdb = function(output, file, doc)
{
    #fix placement of code ndoes
    nodestomove = xpathSApply(output, "//x:code[@in.last.para=='TRUE']", namespaces = c(r="http://www.r-project.org"))
    for(nd in nodestomove)
    {
        addChildren(getSibling(nd, after = FALSE), nd)
    }

    saveXML(output, file = file)
}

finishRmd = finishRdb =   function(output, file, doc) stop("Not implemented yet")


DefaultFinishers <- list(
    rmd = finishRmd,
    md = finishMD,
    ipynb = finishIPyNB,
    html = finishHTML,
    rdb = finishRdb,
    pdf = finishPDF,
    tex = finishTex
    )

getDefaultFinish = function(format)
{
    finish = NULL
    if(!is.null(format))
        finish = DefaultFinishers[[tolower(format)]]
    
    if(is.null(finish))
        stop(sprintf("no default output file finishing method found for format %s", tolower(format)))
    finish
}


