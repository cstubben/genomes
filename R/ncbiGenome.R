ncbiGenome<-function(term, refseq = FALSE)
{
   db  <- "genome"
   x <- esearch( term, db, verbose=FALSE)
   # genome_nuccore link is default
   elink <- elink(x, db="nuccore")
   # get reference and derived genbank
   x1 <- esummary(elink)

   #  get Other INSDC Genome Sequences
   x2 <-  try( esummary( elink ( elink, db="nuccore",  linkname="nuccore_nuccore_samespecies_rsgb")))
   if(class(x2) != "try-error"){  
      x1 <- rbind(x1,x2)
   }
   colnames(x1) <- c("id", "acc", "name" ,    "defline" , "gi", "created", 
        "updated", "flags", "taxid" , "size", "status", "replace", "comment")
   # drop reference (duplicate)   
   if( !refseq){
       x1 <-subset(x1, !x1$defline %like% '*\\|ref\\|*')
   }
   print(paste(nrow(x1), "sequences found"))
     
      # format dates
      x1$created <- as.Date(substr(x1$created, 1,10), "%Y/%m/%d")
      x1$updated <- as.Date(x1$updated,"%Y/%m/%d")

      x1<- x1[, c(2,3, 6, 10,9, 5, 11, 1,4,7,8,12,13)] 
   
      # size and taxid are characters
      x1$size  <- as.numeric(x1$size)
      x1$taxid <- as.numeric(x1$taxid)
   
      # x1 <- x1[ order(x1$name),]
      # rownames(x1) <- 1:nrow(x1)
      ##  add class
      class(x1) <- c("genomes", "data.frame")
      ## save date for updates -then save term??
      attr(x1, "date") <- Sys.Date()
      attr(x1, "term") <-term
      x1
 
}
