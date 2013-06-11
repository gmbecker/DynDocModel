# we need to be able to detect both the code changing (in a meaningful way, perhaps after parsing?) as well as the inputs ot the codeblocks changing.

#for some reason we need to parse code and then convert it back to a character in order to get the same hashes back (via digest::digest() )
#> digest(as.character(parse(text="x=5;\ny=17;plot(x,y)")))
#[1] "dd7229fb77f281d4239b5aa30617db68"
#> digest(as.character(parse(text="x=5;y=17;plot(x,y)")))
#[1] "dd7229fb77f281d4239b5aa30617db68"
#> digest(as.character(parse(text="x=5;\ny=17;plot( x    ,y )")))
#[1] "dd7229fb77f281d4239b5aa30617db68"
#> 

#I'm thinking we will hash a list containing the "parsed" code as well as the current values of the input variables. If that hash is the same as the cached verson we can be sure, up to the uniqueness of the hash, that the cache is safe. If not, either the code or inputs have changed and we need to run it again


