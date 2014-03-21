
getShape = function(element)
  {
    if(is(element, "DecisionElement"))
      "triangle"
    else if(is(element, "TaskElement"))
        "diamond"
    else if (is(element, "ContainerElement"))
      "rectangle"
    else
      "ellipse"
  }


makeStructureGraph = function(doc, taskpalette = c("green", "lightblue", "lightgreen",  "blue"), ...)
{

        
    curcell = 2
    graphlist = list(list(edges=numeric()))
    level = 1
    parentlist = 1
    branchstart = numeric()
    shapes = "ellipse"
    taskdepth = 0
    colors = character()
    labels = "start"
    detail_levels = data.frame(lastcell = curcell, level = 1)
    lastlev = 1

#recursive function to calculate (non-directed) edges between nodes in the document graph.
# if element is a branchset, returns the indexes of the nodes written at the bottom of each branch, otherwise returns the index of the last node written
    .processElement = function(element, branchparent = FALSE, struct_only=FALSE)
      {
          if(!is(element, "ContainerElement"))
              return(parentlist)

          ret = curcell
          #tasks, decisions, and alternatives
                                        #always draw the current nodeta
          parentlist <<- parentlist[parentlist != curcell]
          tmplist = list(edges = parentlist)
                                        #   else
                                        #    tmplist = list(edges = NULL)
       # ret = curcell
          graphlist[[curcell]] <<- list(edges = numeric())
          shapes[curcell] <<- getShape(element)
          labels[curcell] <<- getId(element)
          for(i in parentlist)
              graphlist[[i]]$edges <<- c(graphlist[[i]]$edges, curcell)
          
if(FALSE)
{
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
 }         
          
          
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
          if(is(element, "DecisionElement") )#|| is_parallelTask(element))
          {
                 #same parent for all branch children (side-by-side), we get this via branchparent = TRUE
              inds = unlist(sapply(element$children, .processElement, branchparent = TRUE))
              inds = inds[inds>0]
                                        #the next node needs to connect to all the nodes at the end of the branches
              parentlist <<- inds
              ret = inds
          }
          else if (is(element, "ContainerElement"))
          {
                                        #new parent for each child (sequential)
              
              for(el in element$children)
              {
                  ret = .processElement(el)#, branchparent = FALSE)
              }


       
              if(branchparent)
                  parentlist <<- tmp
              else 
#                  parentlist <<- curcell
                  parentlist <<- ret
              if (is(element, "AltElement") && is_termBranch(element))
                  ret = -1
          }
if(is(element, "TaskElement"))
    taskdepth <<- taskdepth - 1


          ret
      }
    
    for(el in doc$children)
      .processElement(el)
graphlist[[curcell]] = list(edges = numeric())
    shapes = c(shapes, "ellipse")
colors = c("white", colors, "white")
labels = c( labels, "end")
for(i in parentlist)
    graphlist[[i]]$edges = c(graphlist[[i]]$edges, curcell)

#nms = as.character(seq(along=graphlist))
nms = labels
inds = nchar(nms)==0
nms[inds] = as.character(seq(along=graphlist))[inds]
    names(graphlist) = nms
    names(shapes) = nms
    names(colors) = nms
    ret = graphNEL(nodes = names(graphlist), edgeL = graphlist, edgemode = "directed")
#    widths = rep(max(1*nchar(nms)), times=length(nms))
    widths = 140 + 120*(nchar(nms)/max(nchar(nms)))
    heights = ifelse(shapes %in% c("diamond", "triangle"), 250, 160)
    #fixed = rep(FALSE, times=length(nms))
fixed = rep(TRUE, times=length(nms))
    names(widths) = nms
    cex = rep(.7 +.05*(max(nchar(nms)) - nchar(nms)), times = length(nms))
names(cex) = nms
fontsize = 15 + 15*(1-nchar(nms)/max(nchar(nms)))
names(fontsize) = nms
#    nodeRenderInfo(ret) = list(shape = shapes, fill="red")

  # lout = layoutGraph(ret, nodeAttrs = list(width=widths))
 lout = layoutGraph(ret)#, nodeAttrs = list(width=widths))
 #lout = layoutGraph(ret, nodeAttrs = list(fontsize = fontsize))
    nodeRenderInfo(lout) = list(shape = shapes, fill=colors, fontsize=fontsize, lWidth=widths/2, rWidth=widths/2, height=heights)# cex = cex)#, fixedsize=fixed)
    renderGraph(lout)
    lout
    
}
    

makeDocumentGraph = function(doc, taskpalette = c("green", "lightgreen", "lightblue", "blue"), struct_only = FALSE, ...) #taskpalette = brewer.pal(6, "GnBu"), ...)
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
    .processElement = function(element, branchparent = FALSE, struct_only=FALSE)
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
        if(is(element, "DecisionElement") || is_parallelTask(element))
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
            if (is(element, "AltElement") && is_termBranch(element))
                ret = -1
            else
                ret = curcell
            curcell <<- curcell + 1
                
        }
        
        ret
      }
    


    
    for(el in doc$children)
      .processElement(el)
    nms = as.character(seq(along=graphlist))
    names(graphlist) = nms
    names(shapes) = nms
    names(colors) = nms
    fx = rep(FALSE, times = length(nms))
    names(fx) = nms
    fsize = rep(10, times = length(nms))
    names(fsize) = nms
    widths = .2* 
    ret = graphnel(nodes = names(graphlist), edgel = graphlist, edgemode = "directed")
#    noderenderinfo(ret) = list(shape = shapes, fill="red")

    lout = layoutgraph(ret)
    noderenderinfo(lout) = list(shape = shapes, fill=colors, fixedsize = fx, cex = fsize)
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
      
      
      


