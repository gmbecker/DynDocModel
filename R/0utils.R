doListDispatch = function(cl, lst, default = NULL,  classMap = NULL)
    {
        
        if(is.null(default))
            default = lst[["ALL"]]
        if(is.null(default))
            default = lst[["default"]]
        
        meth = default
        
        if(cl %in% names(lst))
            meth = lst[[cl]]
        else if (cl %in% names(classMap))
            meth = lst[[classMap[[cl]]]]
        else
            {
            #recreating the wheel here in terms of dispatch based on inheritence.
            #Probably not a good idea but I don't see how i can leverage the existing S4 machinery...
                found = sapply(names(lst), function(nm)
                    {
                        if(!isClass(nm) && !is.null(classMap))
                            {
                                inMap = which(nm == classMap)
                                if(length(inMap))
                                    nm = names(classMap)[which]
                            }
     
                        ext = extends(cl, nm, fullInfo = TRUE)
                        if(is.logical(ext))
                            -1
                        else
                            ext@distance
                    })
                found = found[found>0]
                
                if(length(found))
                    {
                        cl = names(found)[which.min(found)]
                             if(cl %in% names(lst))
                                 meth = lst[[cl]]
                             else if (cl %in% names(classMap))
                                 meth = lst[[classMap[[cl]]]]
                    }
            }

        if(is.null(meth))
            stop(sprintf("No method found for class %s in list of methods for classes %s, and no default specified", cl, paste(names(lst), collapse=", ")))
        meth
    }

#precedence f1 > f2 > defaults
combineFormatters = function(fl1, fl2)
{
    formatters = c(fl1, fl2)
    formatters = formatters[!duplicated(names(formatters))]
    formatters
}


is_termBranch = function(br)
{
    attrs = dyndoc_attrs(br)
    if("terminal" %in% names(attrs) && attrs[["terminal"]])
        TRUE
    else
        FALSE
}



is_selfOrEl = function(obj, cl)
{
    is(obj, cl) || (is(obj, "ElementInstance") && is(obj$element, cl))
}


removeFancyQuotes = function(content)
{

    gsub("(\u201c|\u201d)", "\"",  content, useBytes = TRUE)

}

insertFancyQuotes = function(content)
{
    content = gsub("\"([^\"]*)\"", replacement= "\u201c\\1\u201d", content, useBytes = TRUE)
    single = grep("^[^\"]*\"[^\"]*$", content, useBytes = TRUE)
    if(length(single == 1))
        content[single] = gsub("\"", "\u201c", useBytes = TRUE, content[single])
    else if(length(single) > 1)
    {
        for(i in seq(1, length(single), by=2))
        {
            content[ single[i] ] = gsub("\"", "\u201c", useBytes = TRUE, content[single[i]])
            content[ single[i+1] ] = gsub("\"", "\u201d", useBytes = TRUE, content[single[i+1]])
        }
    }
    content
            

}

detailLevel = function(el)
{
    if(is(el, "DynDoc") || is(el, "DocInstance"))
       return(1)
    if(is(el, "ElementInstance"))
        el = el$element
    #default is 1
    ret = 1
    if("detail" %in% names(el$attributes))
        ret = el$attributes[["detail"]]
    ret
}

is_parallelTask = function(el)
{
    paral = dyndoc_attrs(el)$parallel
    is_selfOrEl(el, "TaskElement") && !is.null(paral) && paral
}
