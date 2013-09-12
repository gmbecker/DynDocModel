setOldClass("trellis")
setOldClass("ggplot")
setClass("ElementList", contains="list")
setClassUnion("ListOrNull", members = c("list", "NULL"))


setClass("FormattedOutput", representation(value = "ANY", format = "character", info = "list"))
setClass("FormattedOutputList", contains = "list", valid = function(x) all(sapply(x, is, "FormattedOutput")))



setClass("WidgetsList", contains="list")
setClass("IWidget", representation(var = "character",
                                        #I think we want to specify widget by class, but I'll leave this here for custom widgets?
                                   widget = "character",
                                   linenum = "integer",
                                   default = "ANY",
                                   additional.info = "list"))

setClass("IWidgetSlider", representation(range = "numeric",
                                         step = "numeric"),
         contains = "IWidget")


setClass("IWidgetTextbox", contains = "IWidget")
setClass("IWidgetIntTextbox", contains = "IWidgetTextbox")
setClass("IWidgetNumTextbox", contains = "IWidgetTextbox")

dynDoc = setRefClass("DynDoc", fields = list(
                                   .envir = "environment",
                                   envir = function(value)
                                   {
                                       if(missing(value))
                                           .envir
                                       else
                                           .envir <<- value
                                   },
                                   .elements = "ElementList",
                                   elements = function(value)
                                   {
                                       if(missing(value))
                                           .elements
                                       else
                                           {
                                               if(is(value, "DocElement"))
                                                   value = as(list(value), "ElementList")
                                               value = as(sapply(seq(along = value), function(i)
                                                   {
                                                       el = value[[i]]
                                                       el$parent = .self
                                                       el$posInParent = i
                                                       el
                                                   }), "ElementList")
                                               .elements <<- value
                                               
                                           }
                                   },
                                   metadata = "ListOrNull",
                                   formatSpecific = "ListOrNull",
                                   .testbit = "ANY",
                                   cacheEngine = "CachingEngine"),
    methods = list(
        addChild = function(newel)
        {
            newpos = length(elements)+1
            newel$posInParent = newpos
            newel$parent = .self
            elements[[newpos]] <<- newel
            newel
        },
        insertChildren = function(elList, startPos)
        {
            if (is(elList, "DocElement"))
                elList = as(list(elList), "ElementList")
            
            if(!is(elList, "ElementList"))
                stop("elList does not seem to be an ElementList. Unable to insert children")
            if(startPos > length(elements))
                return(sapply(elList, function(el) .self$addChild(el)))
            afterinds = seq(startPos, length(elements))
            elsafter = elements[afterinds]
            elsbefore = elements[-afterinds]
            .self$elements = as(c(elsbefore, elList, elsafter), "ElementList")
                                        #parent/child stuff taken care off in field assignment
            
        },
        removeChild = function(oldel)
        {
            elements <<- elements[-oldel$posInParent]
            oldel$parent = NULL
            oldel$posInParent = 0
        },
        initialize = function(cacheEngine, ...)
        {
            if(missing(cacheEngine))
                cEngine = cachingEngine( base_dir = "./r_caches/", eval_fun = parseWithVis, return_handler = wVGraphicsHandler)
            else
                cEngine = cacheEngine
            callSuper(cacheEngine = cEngine, ...)
        } 
        ))

docElement = setRefClass("DocElement",
    fields = list(
        parent = "ANY",
                                        #metadata = "ListOrNull", #attributes makes more sense, so changing to that
        attributes = "ListOrNull",
        formatSpecific = "ListOrNull",
        posInParent = "numeric",
        id = "character",
        .testbit = "logical",
        cacheEngine = "CachingEngine"),
    
    methods = list(
        initialize = function(obj, .testbit,cacheEngine, ...)
        {
            if(!missing(.testbit))
                {
                    warning("The .testbit field is for internal use only and should not be manually initialized. Ignoring initialization value.")
                }
            args = list(...)
            if(missing(cacheEngine)){
                if("parent" %in% names(args))
                    cEngine = args$parent$cacheEngine
                else
                    cEngine = new("CachingEngine", base_dir = "./r_caches/", return_handler = wVGraphicsHandler, eval_fun = parseWithVis)
            } else {
                cEngine = cacheEngine
            }
            callSuper(.testbit = TRUE, cacheEngine = cEngine, ...)
        })
    )

containerElement = setRefClass("ContainerElement", contains = "DocElement",
    fields = list(
        .envir = "environment",
        envir = function(value)
        {
            if(missing(value))
                .envir
            else
                .envir <<- value
        },
        .children = "ElementList",
        children = function(value)
        {
            if(missing(value))
                .children
            else
                {
                    if(is(value, "DocElement"))
                        value = as(list(value), "ElementList")
                    value = as(sapply(seq(along = value),
                        function(i)
                        {
                            el = value[[i]]
                            el$parent = .self
                            el$posInParent = i
                            el
                        }),
                        "ElementList")
                    
                    .children <<- value
                    .self$resetInOutVars()
                }
        },
        .invars = "character",
        invars = function(value)
        {
            if(missing(value))
                .invars
            else
                .invars <<- value
        },
        .outvars = "character",
        outvars = function(value)
        {
            if(missing(value))
                .outvars
            else
                .outvars <<- value
        }
        ),
    methods = list(
        resetInOutVars = function()
        {
            listin = lapply(.self$children, function(el)
                {
                    if(!(is(el, "CodeElement")||is(el, "ContainerElement")))
                        return(NULL)
                    el$resetInOutVars()
                    el$invars
                })
            listout = lapply(.self$children, function(el)
                {
                    if(!(is(el, "CodeElement")||is(el, "ContainerElement")))
                        return(NULL)
                    el$outvars
                })
            
            retin = character()
            retout = character()
            for(i in seq(along=listin))
                {
                    tmpin = listin[[i]]
                    if(!is.null(tmpin))
                        {
                            retin = c(retin, tmpin[! (tmpin %in% retout) ] )
                            retout = c(retout, listout[[i]])
                        }
                }
            
            .self$invars = retin
            .self$outvars = retout
            
        },
        addChild = function(newel)
        {
            newpos = length(children) + 1
            newel$posInParent = newpos
            newel$parent = .self
            children[[newpos]] <<- newel
            newel
        },
        insertChildren = function(elList, startPos)
        {
            if (is(elList, "DocElement"))
                elList = as(list(elList), "ElementList")
            
            if(!is(elList, "ElementList"))
                stop("elList does not seem to be an ElementList. Unable to insert children")
            if(startPos > length(children))
                return(sapply(elList, function(el) .self$addChild(el)))
            afterinds = seq(startPos, length(children))
            elsafter = children[afterinds]
            elsbefore = children[-afterinds]
            .self$children = as(c(elsbefore, elList, elsafter), "ElementList")
                                        #parent child stuff is now taken care of in activeBinding methods on the fields themselves.
            
        })
    )
taskElement = setRefClass("TaskElement", contains = "ContainerElement")

codeElement = setRefClass("CodeElement", contains = "DocElement",
    fields = list(
        .envir = "environment",
        envir = function(value)
        {
            if(missing(value))
                .envir
            else
                .envir <<- value
        },
        .codehash = "character",
        codehash = function(value)
        {
            if(missing(value))
                .codehash
            else
                .codehash <<- value
        },
        .invars = "character",
        invars = function(value)
        {
            if(missing(value))
                .invars
            else
                .invars <<- value
        },
        .outvars = "character",
        outvars = function(value)
        {
            if(missing(value))
                .outvars
            else
                .outvars <<- value
        },
        .content = "character",
        content = function(value)
        {
            if(missing(value))
                .content
            else
                {
                    .content <<- value
                    valhash = digest(value)#digest(unparse(parse(text=value)))
                                        #the first time content is added during construction, codehash will be character(0)
                    if(length(codehash) && valhash != codehash)
                        {
                            codehash <<- valhash
                            .self$resetInOutVars(force=TRUE)
                        }
                    value
                }
        },
        outputs = "ElementList")
    )

rCodeElement = setRefClass("RCodeElement", contains = "CodeElement",
    fields = list(
        content = function(value)
        {
            if(missing(value))
                .content
            else
                {
                    .content <<- value
                    valhash = digest(unparse(parse(text=value, keep.source=FALSE)))
                    
                    if(length(codehash) && valhash != codehash)
                        {
                            codehash <<- valhash
                            .self$resetInOutVars
                        }
                }
        }),
    methods = list(
        
        resetInOutVars = function(force= FALSE, extra.inputs = NULL, extra.outputs = NULL)
        {
             #Are there any times that this gets called that it doesn't need to update the in and out variables?
          # if(!is(invars, "uninitializedField") &&  !is(outvars, "uninitializedField")  && !force && is.null(extra.inputs) && is.null(extra.outputs))
            #    return()

                             #this will add the `{` function to our functions called, but thats probably ok...?
            code = content
            lastline = code[length(code)]
            if(substr(code[1], 1, 1) != "{" || substr(lastline, nchar(lastline), nchar(lastline)) != "}")
                code2 = c("{", code, "}")
            else
                code2 = code
            if(length(code2) > 1)
                code = paste(code, collapse="\n")
            scr = readScript("", type="R", txt=code2)
            codeInfo = getInputs(scr)[[1]]
            
            invars <<- c(codeInfo@inputs, extra.inputs)
            outvars <<- c( codeInfo@outputs, extra.outputs)
            invisible(list(inputs = invars, outputs = outvars))
        }
        )
 )
        

pyCodeElement = setRefClass("PyCodeElement", contains = "CodeElement",
    methods = list(
        resetInOutVars = function() NULL #currently we don't know how to do this for python
        ))

intCodeElement = setRefClass("IntCodeElement", contains = "CodeElement",
    fields = list(widgets = function(val)
        {
            if(missing(val))
                .widgets
            else
                {
                    if(is(val, "IWidget"))
                        val = list(val)
                    .widgets <<- val
                }
        },
        .widgets = "WidgetsList"))

intRCodeElement = setRefClass("IntRCodeElement", contains = c("IntCodeElement", "RCodeElement"))

intPyCodeElement = setRefClass("IntPyCodeElement", contains = c("IntCodeElement", "PyCodeElement"))

outputElement = setRefClass("OutputElement", contains = "DocElement",
    fields = list(
        codeElement = "CodeElement",
        format = "ANY",
        content="ANY"
        ))

textElement = setRefClass("TextElement", contains="DocElement",
    fields = list(content="ANY"))

mdElement = setRefClass("MDTextElement", contains="TextElement")

latexElement = setRefClass("LatexTextElement", contains = "TextElement")

dbElement = setRefClass("DbTextElement", contains="TextElement")

inlineR = setRefClass("InlineRCode", contains = "RCodeElement")

inlineLatex = setRefClass("InlineLatex", contains = "LatexTextElement")

mixedElement = setRefClass("MixedTextElement", contains = "ContainerElement")

mixedMDElement = setRefClass("MixedMDElement", contains = "MixedTextElement")

mixedDBElement = setRefClass("MixedDBElement", contains = "MixedTextElement")

mixedLatexElement = setRefClass("MixedLatexElement", contains = "MixedTextElement")




branchElement = setRefClass("BranchElement", contains = "ContainerElement")

branchSetElement = setRefClass("BranchSetElement", contains = "ContainerElement")
altImplElement = setRefClass("AltImplElement", contains = "BranchElement")
altImplSetElement = setRefClass("AltImplSetElement", contains = "BranchSetElement")

altMethodElement = setRefClass("AltMethodElement", contains = "BranchElement")
altMethodSetElement = setRefClass("AltMethodSetElement", contains = "BranchSetElement")
altQuestElement = setRefClass("AltQuestElement", contains = "BranchElement")
altQuestSetElement = setRefClass("AltQuestSetElement", contains = "BranchSetElement")

sectElement = setRefClass("SectionElement", contains = "ContainerElement",
    fields = list(
        .title = "character",
        title = function(value)
        {
            if(missing(value))
                .title
            else
                .title <<- value
        }
        ))

headerSectElement = setRefClass("HeaderSectElement", contains = "SectionElement")

#XXX do we want S4 classes or referenceClasses? We want evaluate(mythread) to both return the return value and change the thread, right? Is there a downside to using ReferenceClasses for this?


elementInstance = setRefClass("ElementInstance",
    fields = list(
        element = "DocElement",
        formatters = "list",
        outputs = "list",
        .children = "list",
        envir = "environment",
        children = function(value)
        {
            if(missing(value))
                .children
            else
                .replaceKids(value, .self)
        },
        .parentInstance = "ANY", #want Instances, but recursive class defs in R are hard
        parentInstance = function(value)
        {
            if(missing(value))
                .parentInstance
            else
                {
                    .parentInstance <<- value
                    parent.env(envir) <<- value$envir
                    .cacheEngine <<- value$cacheEngine
                    .parentInstance
                }
        },
        posInParentInst = "numeric",
        cacheEngine = "CachingEngine"),
    methods = list(
        initialize = function(envir, ...)
        {
            if(missing(envir))
                env = new.env()
            else
                env = envir
            callSuper(envir = env, ...)
        })
    )

setRefClass("ContainerInstance", contains = "ElementInstance")
setRefClass("BranchSetInstance", contains = "ElementInstance")

#setIs("Instance", "ElementInstance")

setRefClass("DocInstance",
            fields = list(.children = "list",
                children = function(value)
                {
                    if(missing(value))
                        .children
                    else
                        replaceKids(value, .self)
                },
                envir = "environment",
                parentDoc = "DynDoc",
                formatters = "list",
                cacheEngine = "CachingEngine"),
            methods = list(
                initialize = function(envir, cacheEngine,  ...)
                {
                    if(missing(envir))
                        env = new.env()
                    else
                        env = envir
                    args = list(...)
                    if(missing(cacheEngine)) {
                        if("parentDoc" %in% names(args))
                            cEngine = args$parentDoc$cacheEngine
                        else
                            cEngine = new("CachingEngine", base_dir = "./r_caches/", eval_fun = parseWithVis, return_handler = wVGraphicsHandler)
                    } else {
                        cEngine = cacheEngine
                    }
                    callSuper( envir = env, cacheEngine = cEngine, ...)
                    
                    
                })
            
            )

                                        #setIs("Instance", "DocInstance")
setRefClass("DocThread", contains = "DocInstance")

replaceKids = function(value, self)
    {
        old = self$.children
        sapply(old, function(x)
               {
                   x$parentInstance = NULL
                   x$posInParentInst = numeric()
               })
        value = lapply(value, function(x)
            {
                if(is(x, "DocElement"))
                    new("ElementInstance", element = x)
                else
                    x
            })
        self$.children <- value
        sapply(seq(along = value), function(i)
               {
                   el = value[[i]]
                   el$parentInstance = self
                   el$posInParentInst = i
               })
        self$.children
    }
