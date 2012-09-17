read.glimmer<-function(file)
{
   x  <- readLines(file)
   zz <-textConnection(x[-1])
   x2 <- read.table(zz, stringsAsFactors=FALSE )
   close(zz)
   colnames(x2) <- c("id", "start", "stop",  "frame", "score" )

   x2$strand <- ifelse(x2$start < x2$stop, "+", "-")
   x2$end   <- ifelse(x2$strand=="+", x2$stop, x2$start)
   x2$start <- ifelse(x2$strand=="+", x2$start, x2$stop)

   defline <- strsplit2(x[1],"\\| ",  n=2)
   acc <- strsplit2(x[1])
   acc <- unlist(strsplit(acc, "\\|"))[4]
   acc <- strsplit2(acc, ".", fixed=TRUE)   # remove version to match ptt (without version)
   # bases not listed
   bases <- ncbiNucleotide(acc)$size
 
   ## CREATE GRanges
   x3 <- GRanges(seqnames=acc, ranges=IRanges( x2$start, x2$end), strand=x2$strand, 
        x2[, c(1,4,5) ]  )
   ## needed for genes spanning origin
   if(max(end(x3)) > bases) isCircular(x3)<- TRUE
   seqlengths(x3) <- bases
   metadata(x3) <- list(source=file,  defline=defline, date=Sys.Date() )
   x3

}

