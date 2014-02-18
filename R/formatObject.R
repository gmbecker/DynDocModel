formatObject = function(obj, formatters = list(), default = formatOutput)
    {
        if(any(c("default", "ALL") %in% names(formatters)))
            default = NULL

        meth = doListDispatch(class(obj), formatters, default = default)
        meth(obj, formatters=formatters)

    }
