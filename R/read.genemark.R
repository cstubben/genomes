## read GeneMark.hmm PROKARYOTIC (Version 2.6r) from NCBI


read.genemark<-function(file)
{
   x  <- readLines(file)
   zz <-textConnection(x[-(1:9)])
   x2 <- read.table(zz, stringsAsFactors=FALSE )
   close(zz)
   colnames(x2) <- c("id", "strand", "start", "end", "length", "class")
  
   # check for special characters <
   if( class( x2$start) !="integer"){
      zz <- grep("[^0-9]", x2$start, value=TRUE)
      print(paste("Removed non-numeric characters in start column:", paste(zz, collapse=",")))
      x2$start <- as.integer( gsub(".*?([0-9]+).*", "\\1", x2$start))
   }
   if( class( x2$end) !="integer"){
      zz <- grep("[^0-9]", x2$end, value=TRUE)
      print(paste("Removed non-numeric characters in end column:", paste(zz, collapse=",")))
      x2$end <- as.integer( gsub(".*?([0-9]+).*", "\\1", x2$end))
   }
   acc <- strsplit2(strsplit2(x[2], ": ",  n=2), "\\.")
   # bases and defline not listed
   z <- ncbiNucleotide(acc)
   
   ## CREATE GRanges
   x3 <- GRanges(seqnames=acc, ranges=IRanges( x2$start, x2$end), strand=x2$strand, 
        x2[, c(1,6) ]  )
   ## needed for genes spanning origin -needed?
   if(max(end(x3)) > z$size) isCircular(x3)<- TRUE
   seqlengths(x3) <- z$size
   metadata(x3) <- list(source=file, defline=z$name, date=Sys.Date() )
   x3
}

