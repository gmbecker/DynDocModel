nbformat = 3
nbformat_minor = 0

initIPyNB = function(fname, doc)
{
    out = list()
    out$worksheets = list(list(cells = list()))
    out$nbformat = nbformat
    out$nbformat_minor = nbformat_minor
    if(is(doc, "DocInstance"))
    {
        meta = doc$parentDoc$metadata
        formSpec = doc$parentDoc$formatSpecific
    } else {
        meta = doc$metadata
        formSpec = doc$formatSpecific
    }
    if("ipynb" %in% names(formSpec))
        formSpec = formSpec$ipynb
    else
        formSpec = list()
    
    out$metadata = c(meta, formSpec$ipynb)
    if(!("name" %in% names(out$metadata)))
        out$metadata$name = ""
    out
}

#default HTML path will go through md renders and then have finishHTML call the markdown package.
initMD = initRmd = initHTML = function(fname, doc, ...)
{
    character()
}


initRdb = initTex = function(fname, doc) stop("Not implemented yet")

DefaultInit <- list(
    rmd = initRmd,
    md = initMD,
    ipynb = initIPyNB,
    html = initHTML,
    rdb = initRdb,
    pdf = initTex,
    tex = initTex
    )

getDefaultInit = function(format)
{
    init = NULL
    if(!is.null(format))
        init = DefaultInit[[tolower(format)]]
    
    if(is.null(init))
        stop(sprintf("no default output file initiation method found for format %s", tolower(format)))
    init
}
