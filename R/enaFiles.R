enaFiles<-function(acc, file="submitted")
{
   url1 <- "http://www.ebi.ac.uk/ena/data/view/reports/sra"
   if(file=="submitted"){
      url1 <- paste( url1, "submitted_files", sep="/")
   } else{
      url1 <- paste( url1, "fastq_files", sep="/")
   } 
   n<-length(acc)
   if(n==1){
      read.delim(paste( url1, acc, sep="/") , stringsAsFactors=FALSE)
   } else{
      if(any(duplicated(acc))) { acc<- unique(acc) }
      z<-vector("list", n)
      for(i in 1:n)
      {
         x <-read.delim(paste( url1, acc[i], sep="/") , stringsAsFactors=FALSE)
         print(paste("Listing files from", acc[i]))
         z[[i]]<- x
      }
      do.call("rbind", z)
   }
}
