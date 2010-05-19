image2<-function(x, col=rev(heat.colors(24)),  breaks, log=FALSE, zeroNA=TRUE, sort01=FALSE, all=FALSE,
                 border=NA, box.offset=0.1, round=3, cex, text.cex=1, text.col="black",
                 mar=c(1,3,3,1),  labels=2:3, label.offset=0.1, label.cex=1 )
{
   col <- c("white", col)  # NAs are always white 
   if(!is.numeric(x)){ stop("A numeric matrix is required")}
   # convert vector to matrix if needed -- try image2(1:5)
   if(!is.matrix(x)){ x <- t(as.matrix(x))}      
   if(!missing(cex)){text.cex=cex; label.cex=cex}  ## cex replaces any values in text.cex or label.cex
   op <- par(mar=mar, xpd=TRUE)
   x1 <- ncol(x)                          ## number of columns and rows
   y1 <- nrow(x)
   ## if large matrix, only display 50 rows and 50 columns unless you really want to display ALL 
   if(!all){
      if(x1 > 50) { x <- x[, 1:50]; x1<-50 }
      if(y1 > 50) { x <- x[1:50, ]; y1<-50 }
   }
   x <- x[nrow(x):1, ,  drop=FALSE]       ## flip matrix so top row on botton
   # sort profile string
   if(sort01){
      zz <- x
      # pad with n zeros
      n <- nchar(max(zz)) 
      ## sort binary string
      zz <- apply(zz, 1,  function(x) {paste(sprintf(paste("%0", n, "d", sep=""),x), collapse="")})
      n <- order(zz)
      x <- x[n,]
   }   
   # hack to get three colors needed for cut
   if(length(col) == 1){col <- rep(col, 3)}
   if(length(col) == 2){col <- c(col, col[-1])}
   if(missing(breaks)){ breaks<-length(col) - 1
   } else{ if(length(breaks) != length(col)) { warning("Breaks is not the same length as colors.
Some blocks may be unfilled or some colors may not be used")}}

   ## don't display zeros
   if(zeroNA){  x[x == 0] <- NA }
   
   ## not sure about negative numbers? - ok if zeroNA is FALSE   
   ## check if any values <= 0 and make positive for cut
   checkmin <- min(x, na.rm=TRUE)
   if(checkmin <= 0){x <- x - checkmin + 1}
   ## cut into intervals using log10 transformation
   if(log){z <- cut(log10(x), breaks)}
   else{z <- cut(x, breaks) }
   z2 <- matrix(z, y1, x1)                ## reshape into matrix

   if(checkmin <= 0){ x <- x + checkmin - 1}   
   x <- round(x, round)                   ## round decimal places

   ## PLOT rectangles
   offset <- box.offset/2
   plot(seq(1, x1 + 1, len=2), seq(1, y1 + 1, len=2), type='n', axes=FALSE, ann=FALSE)
   for(i in 1:x1){
      for(j in 1:y1){
        if(is.na(z2[j,i])){n1 <- 1}       ## if element is NA, use first color (default white)
        else{ n1<-match(z2[j,i], levels(z)) + 1}  # else match to cut levels in z2 and use that color (plus 1)   
        rect(i+offset, j+offset, i+1-offset, j+1-offset, border=border, col=col[n1])
        if(!is.na(text.col)){ text(i+.5,j+.5, x[j,i], cex=text.cex, col=text.col)}
      }
   }
   ## rownames (left and right) 
   if(2 %in% labels) text(1-label.offset,    1:y1+.5, rownames(x), pos=2, offset=0, cex=label.cex)  ## on left
   if(4 %in% labels) text(x1+1+label.offset, 1:y1+.5, rownames(x), pos=4, offset=0, cex=label.cex)
   ## colnames (bottom and top)
   if(1 %in% labels) text(1:x1+.5, 1-label.offset,    colnames(x), pos=2, offset=0, srt=90, cex=label.cex)
   if(3 %in% labels) text(1:x1+.5, y1+1+label.offset, colnames(x), pos=2, offset=0, srt=270, cex=label.cex)
   par(op)
 }
