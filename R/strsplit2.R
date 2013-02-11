strsplit2 <-function(x, split=" ", n=1, ...)
{
 y <- strsplit(as.character( x), split=split, ...)
 sapply(y, "[", n=n)
}
