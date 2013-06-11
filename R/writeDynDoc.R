
DefaultRenderers = list(
           ipynb = writeIPyNB,
           rmd = writeRmd,
           rnw = writeRnw,
           rdb = writeRdb
  )

getDefaultRenderer = function(format)
  {
    if(is.null(format))
      stop("No format (NULL) specified. Unable to determine default Renderer.")
    format = tolower(format)
    ret = DefaultRenderers[[format]]
    if(is.null(ret))
      stop(paste0("Format '", format, "' is not associated with a default renderer. Please specify a renderer explicitly."))
    ret

  }

writeDynDoc = function(doc,
  file = NULL,
  format = {if(!is.null(file)) tolower(gsub(".*\\.(*$)", "\\1", file)) else NULL},
  renderer = {if(!is.null(format)) DefaultRenders[[tolower(format)]] else getDefaultRenderer(file, ...)}
  )
  {
    renderer(doc, file, ...)
  }
