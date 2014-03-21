library(DynDocModel)
library(markdown)
setwd("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks")

sidebyside = function(thr1, thr2, file, ...)
{
    out1 = paste("<div id='thread1' style='width:48%;float:left'>\n", writeDynDoc(thr1, output.format = "html", finish.output = function(out, file, doc) markdownToHTML(text = out, fragment.only = TRUE)) , "</div>")

        out2 = paste("<div id='thread2' style='width:48%;float:right;'>\n", writeDynDoc(thr2, output.format = "html", finish.output = function(out, file, doc) markdownToHTML(text = out, fragment.only = TRUE)), "</div>")

    full = paste("<div id='topcontainer' style='float:left;'>", out1, out2, "</div>")
    markdownToHTML(output = file, text =full)
}


doc2 = readDynDoc("~/gabe/checkedout/ComplexDocuments/ipythonNotebooks/FullHousingEDAFixed.ipynb")

tst = dyndoc_rpath(doc2, "//alt[@threadid=='median']")
tst2 = dyndoc_rpath(doc2, "//alt[@threadid=='mean']")

tmp_eval = function(code, env = .GlobalEnv, ...)
{
     ret = evaluate(code, envir =env,
        output_handler =new_output_handler(value=function(x, vis) new("WithVisValue", value=x, visible=vis))
     )
     ret = ret[sapply(ret, function(x) all(class(x) != "source"))]
     as(ret, "OutputList")
 }

parseWithVis = function(code, env, ...)
{
    ret = withVisible(eval(parse(text=code), envir = env))
     new("WithVisValue", value = ret$value, visible = ret$visible)
}


library(evaluate)
medTh = evalDynDoc(getThread(doc2, branches = tst))
meanTh = evalDynDoc(getThread(doc2, branches = tst2))


setwd("~/gabe/checkedout/GabeResearch/Thesis/files/figures/")
sidebyside(medTh, meanTh, "sidebyside.html")
