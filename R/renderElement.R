renderElement = function(obj, renderers = list(), formatters = list(), state = new.env(), converters = list(), default = NULL, return.meth=FALSE)
    {
        #"default" and "ALL" renderers override the default passed in by writeDynDoc based on the output format
        #need the keep the default around through recursive calls, so add it to the list if it was not overridden
        if(any(c("default", "ALL") %in% names(renderers)))
            default = NULL
        else if(!is.null(default))
            renderers$default = default

        meth = doListDispatch(class(obj), renderers, default = NULL)
        if(return.meth)
            meth
        else
            
            meth(obj, renderers = renderers, formatters=formatters, state = state, converters = converters)

    }
