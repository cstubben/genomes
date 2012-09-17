read.ptt <- function( file)
{
   ptt <- readLines(file)
   ## read in TAB delimited file - skip first 2 lines 
   zz <- textConnection(ptt[-(1:2)])
   x <- read.delim(zz, stringsAsFactors=FALSE )
   close(zz)
   ## seq name
   acc <- unlist(strsplit(file, "/"))
   acc <- acc[length(acc)]
   acc <- strsplit2(acc, ".", fixed=TRUE)

   defline <- paste(acc, strsplit2(ptt[1], " - "))
   bases  <- as.numeric( strsplit2(ptt[1], "..", n=2, fixed=TRUE) )

   ##split left..right coords
   y        <- strsplit(x$Location, "..", fixed=TRUE) 
   x$start <- as.numeric(sapply(y, "[", 1))
   x$end   <- as.numeric(sapply(y, "[", 2))
 
   ## cog codes are all empty, so skip column 7
   if( any(x[,7] != "-")){print("WARNING: check column 7: cog codes are not empty")}

   ## change initcap to lower case
   colnames(x) <- tolower( colnames(x))
   colnames(x)[c(4,6)] <- c("gi", "id")   # change pid and synonym

   # ADD NAs to cog and gene 
   x$cog[ x$cog  == "-"] <- NA
   x$gene[x$gene == "-"] <- NA

   # Check for Gene spanning origin -need to set isCircular=TRUE to create GRange!
   n <- which(x$start > x$end)
   if(length(n) == 1){
         if( x$start[n] - x$end[n] > bases *.9){ 
          x$end[n] <- x$end[n] + bases
          print(paste(" Fixed", x$id[n], "spanning origin (added", bases, "bases to end coordinate)"))
           }
   }
   ## CREATE GRanges
   x2 <- GRanges(seqnames=acc, ranges=IRanges( x$start, x$end), strand=x$strand, 
        x[, c(6,3, 4,5,8,9) ]  )
   ## needed for genes spanning origin
   if(max(end(x2)) > bases) isCircular(x2)<- TRUE
   seqlengths(x2) <- bases
   metadata(x2) <- list(source=file,  defline=defline, date=Sys.Date() )
   x2
}
