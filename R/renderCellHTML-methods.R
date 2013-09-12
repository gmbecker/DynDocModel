setMethod("renderCellHTML", "ANY",
          function(node, formatters, ...)
      {
          renderCellMD(node, formatters, ...)
      })
