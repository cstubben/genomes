ncbiGenomeNuc<-function(term, refseq = FALSE, fulltable = FALSE)
{
   db  <- "genome"
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   ## E-search 
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=ncbiGenome.R&email=stubben@lanl.gov&usehistory=y&db=", db, "&term=", 
        gsub(" ", "%20", term), sep = "")

   gp <- readLines(esearch)
   ##  connection errors?
   connecterror<- grepl("ERROR", gp)
   if(any( connecterror)){
     stop("Error connecting to NCBI ", gp[connecterror])
   }
    x <- unlist( strsplit( gp[3], "<[^>]*>" ))
    x <- x[x != ""]
    if (!x[1] > 0) { stop("No matches to ", term, " found") }
    if( x[1]==1){ print("Matches 1 genome")} else{ print(paste( "Matches", x[1], "genomes"))  }            
   querykey <- x[4]
   webenv   <- x[5]

   ## E-Link using neighbor history
   elink <- paste(url, "elink.fcgi?dbfrom=", db,
           "&db=nuccore&cmd=neighbor_history&linkname=genome_nuccore&query_key=", 
            querykey, "&WebEnv=", webenv, sep = "")
   gp2 <- readLines(elink)
   ## lots of \t
   gp2 <- gsub("\t", "", gp2)     
   n1 <- grep("<QueryKey>", gp2)
   n2 <- grep("<WebEnv>", gp2)
   querykey2 <- gsub("<[^>]*>", "", gp2[n1])
   webenv2   <- gsub("<[^>]*>", "", gp2[n2])

   #  A) get reference and derived genbank

   esum <- paste(url, "esummary.fcgi?db=nuccore&query_key=", querykey2, "&WebEnv=", webenv2, sep = "")
   gp3 <- readLines(esum)
   ##  PARSE XML
   doc <- xmlRoot(xmlParse(gp3))
   ## get values for Id and 18 Items
   x1 <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
   #transpose table
   x1 <- t(x1)

    #  get Other INSDC Genome Sequences
   if(!refseq){
     elink2 <- paste(url, "elink.fcgi?dbfrom=nuccore" ,
           "&db=nuccore&cmd=neighbor_history&linkname=nuccore_nuccore_samespecies_rsgb&query_key=", 
            querykey2, "&WebEnv=", webenv2, sep = "")
      gp2 <- readLines(elink2)
      ## lots of \t
      gp2 <- gsub("\t", "", gp2)

      n1 <- grep("<QueryKey>", gp2)
      n2 <- grep("<WebEnv>", gp2)
      querykey3 <- gsub("<[^>]*>", "", gp2[n1])
      webenv3   <- gsub("<[^>]*>", "", gp2[n2])

      ## E-summary
      esum <- paste(url, "esummary.fcgi?db=nuccore&query_key=", querykey3, "&WebEnv=", webenv3, sep = "")
      gp3 <- readLines(esum)

      ##  PARSE XML
      doc <- xmlRoot(xmlParse(gp3))
      ## get values for Id and 18 Items
      x2 <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
      #transpose table
      x2 <- t(x2)
      x1 <- rbind(x1,x2)
   }

     rownames(x1) <- NULL
      colnames(x1) <- c("id", "acc", "name" ,    "defline" , "gi", "created", 
        "updated", "flags", "taxid" , "size", "status", "replace", "comment")
      x1 <- as.data.frame(x1, stringsAsFactors=FALSE)

     if(refseq){
         x1 <-subset(x1, x1$defline %like% '*\\|ref\\|*')
   if( nrow(x1)==1){ print("1 Reference sequence found")
     }else{  print(paste(nrow(x1), "Reference sequences found"))}
     }else{
         x1 <-subset(x1, !x1$defline %like% '*\\|ref\\|*')
         print(paste(nrow(x1), "GenBank sequences found"))
     }
     
      # format dates
      x1$created <- as.Date(substr(x1$created, 1,10), "%Y/%m/%d")
      x1$updated <- as.Date(x1$updated,"%Y/%m/%d")

      x1<- x1[, c(2,3, 6, 10,9, 5, 11, 1,4,7,8,12,13)] 
      if (!fulltable) {
          x1<-   x1[, 1:6]    
      }

      # size and taxid are characters
      x1$size  <- as.numeric(x1$size)
      x1$taxid <- as.numeric(x1$taxid)
   
      x1 <- x1[ order(x1$name),]
      rownames(x1) <- 1:nrow(x1)
      ##  add class
      # class(x1) <- c("genomes", "data.frame")
      ## save date for updates -then save term??
      # attr(x1, "date") <- Sys.Date()
      # attr(x1, "term") <-term
      x1
 
}
