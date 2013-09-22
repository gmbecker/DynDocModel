library(rpath)
library(DynDocModel)

ipn= readIPyNotebook(system.file("documents/notebook.ipynb", package="DynDocModel"))
makeAltImplSet(ipn[2:4], ipn[5:7])


ddocnames = function(obj)
{
    if(is(obj, "DynDoc"))
        kids = obj$elements
    else if(any(sapply( c( "DocInstance", "DocThread", "ContainerElement"), function(cl) is(obj, cl))))
        kids = obj$children
    else
        kids = list()
    res = sapply(kids, function(x) if(is(x, "ElementInstance")) class(x$element) else class(x))
    res
}

rpath(ipn, "/AltImplSetElement/AltImplElement[2]", names_fun = ddocnames)
rpath(ipn, "/*/AltImplElement[2]", names_fun = ddocnames)


ddocnames2 = function(obj)
{
    if(is(obj, "DynDoc"))
        kids = obj$elements
    else if(any(sapply( c( "DocInstance", "DocThread", "ContainerElement"), function(cl) is(obj, cl))))
        kids = obj$children
    else
        kids = list()
    res = sapply(kids, function(x) if(is(x, "ElementInstance")) class(x$element) else class(x))
    doRevAbbrevType(res)
}

rpath(ipn, "/altimplset/altimpl[2]", names_fun = ddocnames2)

