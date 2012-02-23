ncbiSubmit<-function(term, db="nuccore", retmax=1000) 
{
   # USE E-Post?
   if(db=="genome"){stop("E-fetch no longer supports retrievals from the Genome database")}
   if(length(term) > 1){ term  <- paste(term, collapse = ",") }  # comma-separated list of accessions?
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=term2gbk.R&email=stubben@lanl.gov&usehistory=y&db=", 
                           db, "&term=", gsub(" ", "%20", term), sep = "")
   gb <- readLines(esearch)
   connecterror <- grepl("ERROR", gb)
   if (any(connecterror)) {
      stop("Error connecting to NCBI ", gb[connecterror])
   }
   x <- unlist( strsplit( gb[3], "<[^>]*>" ))
   x <- x[x != ""]
   if (x[1] == 0) {
        stop("No matches to ", term, " found", call. = FALSE)
   }
   # limit records??  or USE an efetch loop
   if(as.numeric(x[1]) > retmax){print(paste("WARNING: More records found than retmax value. Only first", 
                                                retmax, "of", as.numeric(x[1]), "records will be downloaded"))}
#   efetch <- paste(url, "efetch.fcgi?db=", db, "&retmax=", retmax, "&rettype=gb&seq_start=1&seq_stop=60&query_key=",  x[4], "&WebEnv=", x[5], sep = "")
# Feb 17, 2012 - new Efetch changes - XML is now default which cause error
    efetch <- paste(url, "efetch.fcgi?db=", db, "&retmax=", retmax, 
                     "&rettype=gb&retmode=text&seq_start=1&seq_stop=60&query_key=",  
                      x[4], "&WebEnv=", x[5], sep = "")
 

   gb <- readLines(efetch)
   n <- c( grep("^LOCUS", gb), length(gb) )

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
