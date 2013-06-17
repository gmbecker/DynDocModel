##  $ md      :List of 4 
##   ..$ chunk.begin: chr "^\\s*```+\\s*\\{r(.*)\\}\\s*$"
##   ..$ chunk.end  : chr "^\\s*```+\\s*$"
##   ..$ ref.chunk  : chr "^\\s*<<(.+)>>\\s*$"
##   ..$ inline.code: chr "`r +([^`\n]+)\\s*`"

rchunkbeg = "^\\s*```+\\s*\\{r(.*)\\}\\s*$"
rchunkend = "^\\s*```+\\s*$"
# in makeMixedEl.R rinline =  "`r +([^`\n]+)\\s*`"

readRmd = function(file, ...)
  {
      filetext = readLines(file)
      begins = grep(rchunkbeg, filetext)
      if(begins[1] > 1)
          begins = c(1, begins)
      #ends+1 because we want the ending pattern included in the chunk
      ends = grep(rchunkend, filetext) + 1
      if(ends[length(ends)] < length(filetext))
          ends = c(ends, length(filetext))

      changepts = sort(c(begins, ends ))
      chnum = rep(seq(1, length(changepts)-1), times = diff(changepts, lag = 1))
      #missing category for the last pt
      chnum = c(chnum, max(chnum))
      res = tapply(filetext, chnum, handleRmdChunk, simplify=FALSE)
      doc = new("DynDoc", elements = res)
#      doc$insertChildren(res, 1)
      doc
    }


handleRmdChunk = function(chunk, ...)
    {
        if(length(grep(rchunkbeg, chunk)))
            {
                #remove begining and ending of chunk to leave only code
                cbegin = chunk[1]
                code = chunk[-c(1, length(chunk))]
                codel = makeCodeEl(code, "R", formatSpecific = list(chunk.start =  cbegin))
                codel
            } else if (length(grep(rinlineMD, chunk))) {
                makeMixedEl(chunk, format = "rmd")
                #need a case to detect inline latex
            } else {
                new("MDTextElement", content = chunk)
            }
     }

writeRmd = function(doc, file, ...)
  {
    stop("not yet supported")

  }
