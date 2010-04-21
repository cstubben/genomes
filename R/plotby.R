plotby<-function(x, groupby="status", subset=NA,top=5, labels=FALSE, abbrev=TRUE, flip=NA,
                    legend="topleft", lbty='o', lcol=1, ltitle=NULL, lcex=1, lsort=TRUE,
                    cex=1,ylim=NA, las=1, lwd=1, log="",
                    xlab="Release Date", ylab="Genomes", 
                    type='l', col=c("blue", "red", "green3", "magenta", "yellow"),
                    lty=1:top, pch=c(15:18, 1:3), ...)
{
   ## genomes should have released and name columns!
   if (class(x)[1] != "genomes"){stop("x must be a genomes table")}
 
   if (!missing(subset)) {   
     r <- eval(substitute(subset), x, parent.frame())
     if (!is.logical(r)) 
            stop("'subset' must evaluate to logical")
     x <- x[r, ]
   }
   ## a number or column name...
   if (length(groupby) == 1) {
     # some checks
     if (is.numeric(groupby)) {
       if(groupby > ncol(x)) {
         stop("x only has ", ncol(x), " columns and groupby is ", groupby )
       }
     } else {
       if (!groupby %in% names(x)) {
         stop("No column matching ", groupby)}
     }
     # convert to vector since  [.genomes returns data.frames  
     groups <-  x[ , groupby] 
   } else {
   ## a vector to group by 
     groups <- groupby
     if (!missing(subset)) { groups <- groups[r]}
     if (length(groups) != length(x$released)) {
        stop("Length of groupby vector (", length(groups), ") does not match number of rows in x")}
   }
   x <- data.frame(released=x$released, name=x$name, groupby=groups, stringsAsFactors=FALSE)
   ## removed rows without date  
   x <- x[!is.na(x$released),]
   if (nrow(x) == 0) {stop("No rows to plot")}
   
   ## get top matches (total lines in plot)
   y <- table2(x$groupby)
   y <- head(rownames(y), top)
   x <- x[x$groupby %in% y, ]

   ## IN case groupby is factor, convert to character so empty factors do not show up in legend
   if (is.factor(x$groupby )) { x$groupby <- as.character(x$groupby)}
   if (length(pch) < top) {pch <-rep(pch, length=top)}
   if (length(col) < top) {col <-rep(col, length=top)}
   #--------------------------------------------------------------------------#
   ## PLOT groups using different symbols and label points
   if (labels) {
     ## order by released date
     x <- x[order(x$released, x$groupby, x$name), ]
     rownames(x) <- 1:nrow(x)
     ## abbreviate name?
     if (abbrev) {  x$name <- abbrev(x$name)}
     # flip labels on left or right at midway point between dates by default
     # OR just increase margins and set XPD (write outside margins) to true
     # op<-par(mar=c(5,4.1,4.1,20), xpd=TRUE)
     if (is.na(flip)) {
        x2 <- mean(as.numeric(range(x$released)))
        class(x2) <- "Date"
        flip <- nrow(x[x$released<x2, ])
     }
     n<-flip  
     plot(x$released, 1:nrow(x),  type=type, col="gray70", lty=lty, xlab=xlab, ylab=ylab, las=las, lwd=lwd, ...)
     ## order top groups alphabetically ???
     lnames <- sort(y)
     #RUN loop
     for (i in 1:nrow(x)) {
       ## if groupby is blank, then grep matches everthing - use [1]
       j <- grep(x$groupby[i], lnames)[1]
       points(x$released[i], i, pch=pch[j], col=col[j]) 
       text(  x$released[i], i, x$name[i], pos=ifelse(i>n, 2,4), cex=cex)
     }
     legend(legend[1],legend[2], lnames, col=col, pch=pch, bty=lbty, ncol=lcol, title=ltitle, cex=lcex)
   }
   #--------------------------------------------------------------------------#
   ## GROUP using multilines
   else {
     genomes <- table(x$released, x$groupby)
     genomes <- apply(genomes, 2, cumsum)
     ystart <- 0
     #start log scale at 1
     if (!log == '') {ystart <- 1}
     if (any(is.na(ylim))) { ylim <- c(ystart,max(genomes)) }      
     plot(as.Date(rownames(genomes)), 1:nrow(genomes), type='n', ylim=ylim,
            xlab=xlab, ylab=ylab, las=las, log=log,  ...)
         
     ## order legend by decreasing order of max (or mean?) number in column?
     if (lsort) {
       n <- sort(apply(genomes,2,max, na.rm=TRUE), index.return=TRUE, decreasing=TRUE)$ix
     } else {
       n <- 1:ncol(genomes)
     }
     ## sort groups high to low
     y <- sort(apply(genomes,2,max), index.return=TRUE, decreasing=TRUE)
     ## Loop through groups
     for (i in 1:ncol(genomes) ) {                 
       lines(as.Date(rownames(genomes)), genomes[,n[i] ],lty=lty[i], lwd=lwd, col=col[i], type=type, ...)
     }         
     legend(legend[1],legend[2], colnames(genomes)[n],
           lty=lty, col=col, bty=lbty, ncol=lcol, title=ltitle, cex=lcex, lwd=lwd)
   }
}

