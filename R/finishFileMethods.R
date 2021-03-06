finishIPyNB = function(output, file, doc)
{
    cat(file = file, toJSON(output, .withNames = TRUE))
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
    browser()
    #fix placement of code ndoes
    nodestomove = xpathSApply(output, "//x:code[@in.last.para='TRUE']", namespaces = c(x="http://www.r-project.org"))
    for(nd in nodestomove)
    {
        sib = getSibling(nd, after=FALSE)
        while(xmlName(sib) != "para")
            sib = getSibling(sib, after=FALSE)
        addChildren(sib, nd)
    }

    saveXML(output, file = file)
}

finishRmd =  function(output, file, doc) stop("Not implemented yet")


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
    
    if(is.null(finish)){
        
        warning(sprintf("no specific default  file finishing method found for format %s.", tolower(format)))
       finish =  function(out, file, doc) cat(paste(out, collapse = "\n"), file = file)
    }
    finish
}


