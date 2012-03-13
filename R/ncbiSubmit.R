ncbiSubmit<-function(term, db="nuccore") 
{
   # only protein or nucleotide dbs?
   if(db=="genome"){stop("Efetch no longer supports retrievals from the Genome database")}
   if(length(term) > 1){ term  <- paste(term, collapse = ",") }  

   # CHECK if IDs (and skip esearch)
   if( grepl("^[0-9, ]*$", term)){
     gb <- efetch(term, db, rettype="gb", seq_stop=60)
   }else{
     gb <- efetch(esearch(term, db), rettype="gb", seq_stop=60)
   }
   n <-  grep("^LOCUS", gb)
   if(length(n)==0){stop("No results found")}

   n <- c( n, length(gb) )

   y <- data.frame(acc=character(0), definition=character(0), submitted=character(0) , stringsAsFactors=FALSE)
   class(y$submitted) <- "Date"

   for (i in 1: (length(n) - 1) )
   {
      n1  <- grep("^ACCESSION", gb[n[i]:n[i + 1]], value=TRUE )
      y[i, 1]  <- gsub("ACCESSION *(.*) REGION.*", "\\1", n1)
      n1  <- grep("^DEFINITION", gb[n[i]:n[i + 1]], value=TRUE )
         def <- gsub("DEFINITION *([^,]*).*", "\\1", n1)
         def <- gsub("\\.$", "", def)
      y[i, 2]  <- def
      n1 <- grep("^  JOURNAL   Submitted", gb[n[i]:n[i + 1]], value=TRUE  )
      if(length(n1) == 0) { 
         y[i,3] <- NA
      }else{
         n2 <- gsub(".*?\\((.*)\\).*", "\\1", n1)
         # may be more than 1 submission date
         y[i,3] <- min(as.Date(n2, format = "%d-%b-%Y"))
      }
   }
   y
}
