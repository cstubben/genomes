read.prodigal<-function(file, allScores=FALSE)
{
   x <- readLines(file)
   # row 1 DEFINITION
   x1 <-unlist(strsplit(x[1], ";"))
   x1 <- gsub('"', '', x1)  # remove quotes  
   x1 <- gsub("([^=]*=)(.*)", "\\2", x1)

   bases <- as.numeric(x1[2])
   defline <- strsplit2(x1[3],"\\| ",  n=2)
   acc <- strsplit2(x1[3])
   acc <- unlist(strsplit(acc, "\\|"))[4]
   acc <- strsplit2(acc, ".", fixed=TRUE)  # remove version to match ptt (without version)?
   
   # CDS
   n <- grep('CDS', x)

   ## check - CDS line then features
   if( any(diff(n) != 2)){print("WARNING: check for multiple lines after CDS")}
   
   ## STRAND 
   strand <- ifelse(grepl("complement", x[n]), "-", "+")
      #  complement(69378..>70304)
   start  <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x[n]))
   end    <- as.numeric(gsub(".*?([0-9]+).*$",  "\\1", x[n]))


   ## features -- remove quotes 
   x2 <- gsub('"', '', x[n+1])
   x2 <- strsplit( x2, ";")
   z  <- t(sapply(x2, function(x) { gsub("([^=]*=)(.*)", "\\2", x)}))
   z[,1] <- gsub("ID=", "", z[,1])
   colnames(z) <- c("id", "partial", "start_type", "rbs_motif", "rbs_spacer", "score", "cscore", "sscore", "rscore", "uscore", "tscore")
  
   if(allScores){
      x3 <- data.frame(start, end, strand, z, stringsAsFactors=FALSE)
      for(i in 9:14){class(x3[,i]) <- "numeric"}
   } else{
      # skip score in column 6, then rscore, uscore, tscore
      x3 <- data.frame(start, end, strand, z[,c(1:5, 7:8)], stringsAsFactors=FALSE)
      for(i in 9:10){class(x3[,i]) <- "numeric"}
   }
 
   # CREATE Grange...
   x4 <- GRanges(seqnames=acc, ranges=IRanges( x3$start, x3$end), strand=x3$strand, 
        x3[, -(1:3) ]  )
   ## needed for genes spanning origin - findOverlaps and other functions may not work if isCircular=TRUE
   if(max(end(x4)) > bases) isCircular(x4)<- TRUE
   seqlengths(x4) <- bases
   metadata(x4) <- list(source=file,  defline=defline, date=Sys.Date() )
   x4


}

