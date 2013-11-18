
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
                  alt = "BranchElement",
                  altset = "DecisionElement",
                  altimpls = "AltImplSetElement",
                  altimpl = "AltImplElement",
                  altmeths = "AltMethodSetElement",
                  altmeth = "AltMethodElement",
                  sect = "SectionElement",
                  branch = "BranchElement",
                  branchset = "DecisionElement",
                  any = "DocElement", #any should match all nodes
                  "*" = "DocElement",
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
                  BranchElement = "alt",
                  DecisionElement = "altset",
                  AltImplSetElement = "altimplset",
                  AltImplElement = "altimpl",
                  AltMethodSetElement = "altmethset",
                  AltMethodElement = "altmeth",
                  SectionElement = "sect",
                  DocElement = "*",
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
                  BranchElement = "alt",
                  DecisionElement = "altset",
                  AltImplSetElement = "altset",
                  AltImplElement = "alt",
                  AltMethodSetElement = "altset",
                  AltMethodElement = "alt",
                  SectionElement = "sect",
                  DocElement = "*",
                  cl #by default assume node type was not abbreviated
                  )
       })
}

