
getShape = function(element)
  {
    if(is(element, "DecisionElement"))
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
    detail_levels = data.frame(lastcell = curcell, level = 1)
    lastlev = 1

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

        detlev = if(is.null(element$attributes) || is.null(element$attributes$detail)) 1 else  element$attributes$detail
      

        if(lastlev >  detlev)
        {
            
            j = nrow(detail_levels)
            while(j >=1 )
            {
                if(detail_levels[j, "level"] <= detlev)
                {
                    lowercell = detail_levels[j, "lastcell"]
                    graphlist[[lowercell]]$edges <<- c(graphlist[[lowercell]]$edges, curcell)
                }
                if(detail_levels[j, "level"] == detlev)
                {
                    break()
                }
                j = j - 1
            }
        }
        
        if(detlev %in% detail_levels$level)
            detail_levels[detail_levels$level == detlev, "lastcell"] <<- curcell
        else
            detail_levels <<- rbind(detail_levels, data.frame(level = detlev, lastcell = curcell))
        lastlev <<- detlev
        detail_levels <<- subset(detail_levels, level <= detlev)
        
        
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
        if(is(element, "DecisionElement"))
          {
            #same parent for all branch children (side-by-side), we get this via branchparent = TRUE
            inds = sapply(element$children, .processElement, branchparent = TRUE)
            inds = inds[inds>0]
            #the next node needs to connect to all the nodes at the end of the branches
            parentlist <<- inds
            ret = inds
          }
        else if (is(element, "ContainerElement"))
          {
                                        #new parent for each child (sequential)
         #     browser()
         #   tmp = parentlist
            for(el in element$children)
              {
                ret = .processElement(el)#, branchparent = FALSE)
              }
            
            #end container grouping
        #    curcell <<- curcell + 1
            graphlist[[curcell]] <<- list(edges = numeric())
            shapes[curcell] <<- getShape(element)
            for(r in ret)
                graphlist[[r]]$edges <<- c(graphlist[[r]]$edges, curcell)

            if(taskdepth)
                colors <<- c(colors, taskpalette[taskdepth])
            else
                colors <<- c(colors, "white")
            
            
            if(branchparent)
                parentlist <<- tmp
            else
                parentlist <<- curcell
            
            if(is(element, "TaskElement"))
              taskdepth <<- taskdepth - 1
            if (is(element, "BranchElement") && is_termBranch(element))
                ret = -1
            else
                ret = curcell
            curcell <<- curcell + 1
                
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
#    nodeRenderInfo(ret) = list(shape = shapes, fill="red")

    lout = layoutGraph(ret)
    nodeRenderInfo(lout) = list(shape = shapes, fill=colors)
    renderGraph(lout)
    lout
    
  }

if(FALSE)
  {
#recursive function to calculate (non-directed) edges between nodes in the document graph.
# if element is a branchset, returns the indexes of the nodes written at the bottom of each branch, otherwise returns the index of the last node written
processElement = function(element, branchparent = FALSE)
  {
    
    if(is(element, "DecisionElement"))
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
      
      
      


