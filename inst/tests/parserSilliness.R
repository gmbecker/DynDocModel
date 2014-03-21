library(DynDocModel)
library(roxygen2)
library(tools)

silly_parser = function(file, ...)
{
    p = parse(file, keep.source=TRUE)
    refs = getSrcref(p)
    comments = roxygen2:::comments(refs)
    keep = sapply(comments, function(x) any(nchar(as.character(x))>0))
    refs = refs[keep]
    comments = comments[keep]
    els = list()
    for(i in seq(along=refs))
    {
        rox = do_rd(as.character(comments[[i]]),
                    as.character(refs[[i]]))
        code = paste(as.character(refs[[i]]), collapse="\n")
        els = c(els, new("MDTextElement", content = rox),
                     new("RCodeElement", content = code))
    }
    new("DynDoc", children = els)
}

do_rd = function(com, fun)
{
    rox = paste(paste(com, collapse="\n"), paste(fun, collapse="\n"), sep="\n")
    rox = as.character(rox)
    roc = rd_roclet()
    rd = roc_proc_text(roc, rox)
    fil = tempfile()
    cat(format(rd[[1]]), file=fil)
    #writeLines(rd, con = fil)
    prd = parse_Rd(fil)
    res = character()
    Rd2HTML(prd, out = textConnection("res", "w", local=TRUE))
    res[-seq(1, grep("</head><body>", res) + 1)]
    
    
}

res = silly_parser("~/gabe/checkedout/evaluate/R/replay.r")

thing = evalDynDoc(res)

finish = function(out, file, doc)
{
    style.stupidness = c( paste(readLines(getOption("markdown.HTML.stylesheet")), collapse="\n"), paste(readLines(system.file("html/R.css", package="DynDocModel"))))
    markdownToHTML(output=file, text = out, stylesheet= style.stupidness)
}

library(markdown)
writeDynDoc(thing, "silly_parserout.html", finish.output = finish)


