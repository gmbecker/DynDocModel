finishIPyNB = function(output, file, doc)
    {
        cat(file = file, toJSON(output))
    }

finishRmd = finishRdb = finishHTML = finishTex = finishPDF = function(output, file, doc) stop("Not implemented yet")


DefaultFinishes = list(
    rmd = finishRmd,
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
            finish = DefaultFinishes[[tolower(format)]]

        if(is.null(finish))
            stop(sprintf("no default output file finishing method found for format %s", tolower(format)))
        finish
    }


