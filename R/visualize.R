
getShape = function(element)
  {
    if(is(element, "BranchSetElement"))
      "triangle"
    else if (is(element, "ContainerElement"))
      "rectangle"
    else
      "circle"
  }

makeDocumentGraph = function(doc, taskpalette = c("green", "lightgreen", "lightblue", "blue"), ...) #taskpalette = brewer.pal(6, "GnBu"), ...)
  {

        
    curcell = 1
    graphlist = list()
    level = 1
    parentlist = numeric()
    branchstart = numeric()
    shapes = character()
    taskdepth = 0
    colors = character()

#recursive function to calculate (non-directed) edges between nodes in the document graph.
# if element is a branchset, returns the indexes of the nodes written at the bottom of each branch, otherwise returns the index of the last node written
    .processElement = function(element, branchparent = FALSE)
      {

        #always draw the current node
        tmplist = list(edges = parentlist)
                                        #   else
                                        #    tmplist = list(edges = NULL)
        ret = curcell
        graphlist[[curcell]] <<- list(edges = numeric())
        shapes[curcell] <<- getShape(element)
        for(i in parentlist)
          graphlist[[i]]$edges <<- c(graphlist[[i]]$edges, curcell)
        tmp = parentlist
        parentlist <<- curcell
        curcell <<- curcell + 1
        if(is(element, "TaskElement"))
          taskdepth <<- taskdepth + 1
        if(taskdepth)
          colors <<- c(colors, taskpalette[taskdepth])
        else
          colors <<- c(colors, "white")

        #If the node has children, recursively draw its contents
        if(is(element, "BranchSetElement"))
          {
            #same parent for all branch children (side-by-side), we get this via branchparent = TRUE
            inds = sapply(element$children, .processElement, branchparent = TRUE)
            #the next node needs to connect to all the nodes at the end of the branches
            parentlist <<- inds
            ret = inds
          }
        else if (is(element, "ContainerElement"))
          {
                                        #new parent for each child (sequential)
         #   tmp = parentlist
            for(el in element$children)
              {
                ret = .processElement(el)#, branchparent = FALSE)
              }
            if(branchparent)
              parentlist <<- tmp

            if(is(element, "TaskElement"))
              taskdepth <<- taskdepth - 1
            ret
          }
        ret
      }
    


    
    for(el in doc$elements)
      .processElement(el)
    nms = as.character(seq(along=graphlist))
    names(graphlist) = nms
    names(shapes) = nms
    names(colors) = nms
    ret = graphNEL(nodes = names(graphlist), edgeL = graphlist, edgemode = "directed")
    print(as.list(colors))
    nodeRenderInfo(ret) = list(shape = shapes, fill="red")
   # nodeRenderInfo(ret) = list(fill = colors)
    lout = layoutGraph(ret)
    renderGraph(lout)
    lout
    
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
      
      
      


