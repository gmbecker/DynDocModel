rinlineMD =  "`r +([^`\n]+)\\s*`"

makeMixedEl = function(txt, format, splitter = splitters[[tolower(format)]])
{
    chunks = splitter(txt)
    chunks
}


handleRmdMixed = function(fulltxt)
    {
        oneline = paste(fulltxt, collapse = "\n")
        matches = gregexpr( rinlineMD, oneline )[[ 1 ]]
        lens = c(attr(matches, "match.length"), -1)
        matches = c(matches, nchar(oneline))
        chunks = list()
        pos = 1
        for(i in seq(along = matches))
            {
                tmpMat = matches[i]
                if(pos < tmpMat)
                    chunks[[length(chunks)+1]] = new("MDTextElement", content = substr(oneline, pos, tmpMat - 1))

                tmpLen = lens[i]
                if(tmpLen > 0)
                    chunks[[length(chunks)+1]] = new("InlineRCode", content = substr(oneline, tmpMat+2, tmpMat + tmpLen - 2)) #don't include `r and `
                pos = tmpMat + tmpLen + 1
            }
        new("MixedMDElement", children = chunks)
    }

handleRdbMixed = function(...) stop("not yet implemented")

splitters = list(rmd = handleRmdMixed,
    rdb = handleRdbMixed,
    ipynb = function(...) stop("the core ipython nb format does not support mixed elements")
    )



