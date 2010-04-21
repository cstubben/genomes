table2 <- function(..., n=10)
{
   x1 <- table(...)
   top<-n
   if(is.na(top) | !top > 0 ) {top <- 1000}
   n  <- length(dim(x1))   
   ## if table is a vector
   if (n == 1) {          
     # with 1 element or more  
     if (dim(x1) > 0) {  
       x1 <- matrix(x1, dimnames=list(names(x1), "Total"))
       ## remove zero rows?
       x1 <- x1[rowSums(x1) != 0, , drop=FALSE]
       x1 <- x1[order(x1[ , 1], decreasing=TRUE), , drop=FALSE] 
     } 
   }
   # if table is a 2-d array
   if (n == 2) {
     class(x1)<-"matrix"
     ## remove zero rows and columns?
     x1 <- x1[ , colSums(x1) != 0, drop=FALSE]
     x1 <- x1[   rowSums(x1) != 0, , drop=FALSE]     
     # with 1 column
     if (dim(x1)[2] == 1) {
       x1<-x1[order(x1[,1], decreasing=TRUE), , drop=FALSE ] 
     }
     if (dim(x1)[2]>1) {
       ## add total to rows
       x1 <- addmargins(x1, 2, FUN=list(Total=sum))
       x1 <- x1[order(x1[, "Total"], decreasing=TRUE), ] 
     }
   }
   #if (n > 2)  if more than 3 dimensions, don't do anything
   # return
   if(nrow(x1) > top) {
     x1[1:top, , drop=FALSE]
     } else {
       x1
     }
}
