
#regression test, getThead with document that has a single top-level element that is a decision
test_getThread1Dec = function()
{
      
    text1 = textElement$new(content = "branch 1 content")
    text2 = textElement$new(content = "branch 2 content")
    alt1 = branchElement$new(children = list(text1))
    alt2 = branchElement$new(children = list(text2))
  
    decEl = branchSetElement$new(children = list(alt1, alt2))
    doc = dynDoc$new(children = list(decEl))
    thr1 = getThread(doc)
    checkTrue(length(thr1$children) == 1 && is_selfOrEl(thr1[[1]], "BranchElement")) #should contain just a single branch instance
    checkTrue(thr1[[1]][[1]]$element$content == "branch 1 content")
}

test_getThread1Task = function()
{
    text1 = textElement$new(content = "content 1")
    text2 = textElement$new(content = "content 2")
    task1 = taskElement$new(children = list(text1, text2))
    doc = dynDoc$new(children = list(task1))
    thr1 = getThread(doc)
    checkTrue(length(thr1$children) == 1 && is_selfOrEl(thr1[[1]], "TaskElement")) #should contain just a single branch instance
    checkEquals(sapply(thr1[[1]]$children, function(x) x$element$content),c("content 1", "content 2"))
}

test_getAllThreads = function()
{
    doc = readDynDoc("../documents/NestedBranching.ipynb")
    allthrs = getAllThreads(doc)
    checkTrue(length(allthrs) == 8 && is(allthrs, "ThreadList"), "checking all threads detection on inst/documents/NestedBranching.ipynb")
}

test_getAllThreadsDetail = function()
{
    doc = readDynDoc("../documents/DetailDecision.ipynb")
    det_one = getAllThreads(doc)
    checkTrue(length(det_one) == 2 && is(det_one, "ThreadList"), "checking omission of decisions with detail level higher than active level when generating all threads")
    det_two = getAllThreads(doc, detail_level = 2)
    checkTrue(length(det_two) == 8 && is(det_two, "ThreadList"), "checking inclusion of detail decisions with levels lower than active level.")
}

test_getAllThreadsStartEnd = function()
{
    doc = readDynDoc("../documents/DetailDecision.ipynb")
    start_4 = getAllThreads(doc, start = 4, detail_level = 2)
    start_5 = getAllThreads(doc, start = 5, detail_level = 2)
    checkTrue(length(start_4) == 4 && length(start_5) == 2, "Checking specifying starting position for getAllThreads")
    start_end = getAllThreads(doc, start = 3, end = 4, detail_level = 2)
    checkTrue(length(start_end) == 4, "Checking specifying starting and ending position")
}    
