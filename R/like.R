"%like%" <-function(x, pattern)
{
   pattern <- glob2rx(pattern)
   as.character(x) %in% grep(pattern, x, ignore.case=TRUE, value=TRUE)
}


