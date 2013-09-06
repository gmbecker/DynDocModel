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

        
