strsplit2 <-function(x, split=" ", n=1, ...)
{
 y<-strsplit(x, split=split, ...)
 sapply(y, "[", n=n)
}
