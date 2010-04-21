top<-function(x, n=10)
{
  x %in% names(head(sort(table(x), decreasing=TRUE), n))
}
