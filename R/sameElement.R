sameElement = function(el1, el2)
{
    ret = FALSE
    if(el1$.testbit == el2$.testbit)
    {
        old = el1$.testbit
        el1$.testbit = FALSE
        if(!el2$.testbit)
            ret = TRUE
        el1$.testbit = TRUE
        el2$.testbit = TRUE
    }
    ret
}
