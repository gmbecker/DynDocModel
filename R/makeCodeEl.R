makeCodeEl = function(code, lang, formatSpecific = NULL, inputs = NULL, outputs = NULL)
    {
        switch(lang,
               R = {
            #       if(is.null( inputs ) || is.null( outputs ) )
               #    {
                           #this will add the `{` function to our functions called, but thats probably ok...?
               #            lastline = code[length(code)]
               #            if(substr(code[1], 1, 1) != "{" || substr(lastline, nchar(lastline), nchar(lastline)))
               #                code2 = c("{", code, "}")
               #            else
               #                code2 = code
                #           scr = readScript("", type="R", txt=code2)
                #           codeInfo = getInputs(scr)[[1]]
                #           
                #           inputs = codeInfo@inputs
                #           outputs = codeInfo@outputs
                #       }
                  # hash = digest(unparse(parse(text = code, keep.source = FALSE)))
                   

                   #new("RCodeElement", hash = hash, inputs = inputs, outputs = outputs, content = code, formatSpecific = formatSpecific)
                   #hash and inputs handled by the class automatically
                   new("RCodeElement", content = code, formatSpecific = formatSpecific)
               },
               python = {
                   new("PyCodeElement", hash = digest(code), inputs = inputs, outputs = outputs, content = code, formatSpecific = formatSpecific)
               })
    }
