
makeDocumentGraph = function(doc, ...)
  {

        
    curcell = 1
    graphlist = list()
    level = 1
    parentlist = numeric()
    branchstart = numeric()

#recursive function to calculate (non-directed) edges between nodes in the document graph.
# if element is a branchset, returns the indexes of the nodes written at the bottom of each branch, otherwise returns the index of the last node written
    .processElement = function(element, branchparent = FALSE)
      {
        
        if(is(element, "BranchSetElement"))
          {
            branchstart <<- c(branchstart, curcell)
                                        #same parent for all branch children (side-by-side), we get this via branchparent
            inds = sapply(element$children, .processElement, branchparent = TRUE)
                                        #the next node needs to connect to all the nodes at the end of the branches
            parentlist <<- inds
        inds
          }
        else if (is(element, "ContainerElement"))
          {
                                        #new parent for each child (sequential)
            tmp = parentlist
            for(el in element$children)
              {
                ret = .processElement(el)#, branchparent = FALSE)
              }
            if(branchparent)
              parentlist <<- tmp
            
            ret
          }
        else
          {
                                        #    if(parent >0)
            tmplist = list(edges = parentlist)
                                        #   else
                                        #    tmplist = list(edges = NULL)
            written = curcell
            graphlist[[curcell]] <<- list(edges = numeric())
            for(i in parentlist)
              graphlist[[i]]$edges <<- c(graphlist[[i]]$edges, curcell)
            parentlist <<- curcell
            curcell <<- curcell + 1
            written
          }
      }
    


    
    for(el in doc$elements)
      .processElement(el)
    names(graphlist) = seq(along=graphlist)
    ret = graphNEL(nodes = names(graphlist), edgeL = graphlist, edgemode = "directed")
    
    
  }

if(FALSE)
  {
#recursive function to calculate (non-directed) edges between nodes in the document graph.
# if element is a branchset, returns the indexes of the nodes written at the bottom of each branch, otherwise returns the index of the last node written
processElement = function(element, branchparent = FALSE)
  {
    
    if(is(element, "BranchSetElement"))
      {
        #same parent for all branch children (side-by-side), we get this via branchparent
        inds = sapply(element$children, processElement, branchparent = TRUE)
        #the next node needs to connect to all the nodes at the end of the branches
        parentlist <<- inds
        inds
      }
    else if (is(element, "ContainerElement"))
      {
                                        #new parent for each child (sequential)
        tmp = parentlist
        for(el in element$children)
          {
           ret = processElement(el, branchparent = FALSE)
          }
        if(branchparent)
          ret = tmp
        
        ret
      }
    else
      {
    #    if(parent >0)
        tmplist = list(edges = parentlist)
                                        #   else
      #    tmplist = list(edges = NULL)
        written = curcell
        graphlist[[curcell]] <<- tmplist
        parentlist <<- curcell
        curcell <<- curcell + 1
        written
      }
  }

}    
      
      
      


