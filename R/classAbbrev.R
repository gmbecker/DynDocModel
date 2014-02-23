
abbrevToClass = function(types)
{
    sapply(types, function(ty)
       {
           switch(ty,
                  code = "CodeElement",
                  rcode = "RCodeElement",
                  pycode = "PyCodeElement",
                  text = "TextElement",
                  markdown = "MDTextElement",
                  md = "MDTextElement",
                  docbook = "DbTextElement",
                  db = "DbTextElement",
                  task = "TaskElement",
                  output = "OutputElement",
                  alt = "AltElement",
                  alternative = "AltElement",
                  altset = "DecisionElement",
                  altimpls = "AltImplSetElement",
                  altimpl = "AltImplElement",
                  altmeths = "AltMethodSetElement",
                  altmeth = "AltMethodElement",
                  sect = "SectionElement",
                  branch = "AltElement",
                  branchset = "DecisionElement",
                  decision = "DecisionElement",
                  dec = "DecisionElement",
                  any = "DocElement", #any should match all nodes
                  "*" = "DocElement",
                  thread = "DocThread",
                  document = "DynDoc",
                  ty #by default assume node type was not abbreviated
                  )
       })
}

#1-1 abbreviation
classToAbbrev = function(classes)
{
    sapply(classes, function(cl)
       {
           switch(cl,
                  CodeElement = "code",
                  RCodeElement = "rcode",
                  PyCodeElement = "pycode",
                  TextElement = "text",
                  MDTextElement = "markdown",
                  DbTextElement = "docbook",
                  TaskElement = "task",
                  OutputElement = "output",
                  AltElement = "alt",
                  DecisionElement = "decision",
                  AltImplSetElement = "altimplset",
                  AltImplElement = "altimpl",
                  AltMethodSetElement = "altmethset",
                  AltMethodElement = "altmeth",
                  SectionElement = "sect",
                  DocElement = "*",
                  DocThread = "thread",
                  DynDoc = "document",
                  cl #by default assume node type was not abbreviated
                  )
       })
}



#All branching structures mapped to altset and alt (not 1-1)
classToAbbrev2 = function(classes)
{
    sapply(classes, function(cl)
       {
           switch(cl,
                  CodeElement = "code",
                  RCodeElement = "rcode",
                  PyCodeElement = "pycode",
                  TextElement = "text",
                  MDTextEleemnt = "markdown",
                  DbTextElement = "docbook",
                  TaskElement = "task",
                  OutputElement = "output",
                  AltElement = "alt",
                  DecisionElement = "decision",
                  AltImplSetElement = "decision",
                  AltImplElement = "alt",
                  AltMethodSetElement = "decision",
                  AltMethodElement = "alt",
                  SectionElement = "sect",
                  DocElement = "*",
                  DocThread = "thread",
                  DynDoc = "document",
                  cl #by default assume node type was not abbreviated
                  )
       })
}

