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
    cat(file = file, pandoc_convert(output, out_format="latex"))
    if(inherits(file, "connection"))
        close(file)
}


finishRmd = finishRdb =  finishPDF = function(output, file, doc) stop("Not implemented yet")


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


