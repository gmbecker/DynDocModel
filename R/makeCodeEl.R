makeCodeEl = function(code, lang, formatSpecific = NULL, inputs = NULL, outputs = NULL)
    {
        switch(lang,
               R = {
                   #hash and inputs handled by the class automatically
                   new("RCodeElement", content = code, formatSpecific = formatSpecific)
               },
               python = {
                   new("PyCodeElement", hash = digest(code), inputs = inputs, outputs = outputs, content = code, formatSpecific = formatSpecific)
               })
    }
