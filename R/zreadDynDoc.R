
DefaultParsers = list(
           ipynb = readIPyNotebook,
           rmd = readRmd,
           rnw = readRnw,
           rdb = readRdb
  )

getDefaultParser = function(file)
  {
    ext= tolower(gsub(".*\\.(.*$)", "\\1", file))
    ret = DefaultParsers[[ext]]
    if(is.null(ret))
      stop(paste0("File extension '", ext, "' is not associated with a default parser. Please specify a parser explicitly or rename the file."))
    ret

  }

readDynDoc = function(file, parser = getDefaultParser(file), ...)
  {
    parser(file, ...)
  }
