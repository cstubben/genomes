ncbiPubmed<-function(term)
{ 
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   # use EPost if term is a numeric vector 
   if (is.numeric(term) & length(term) > 1) {   
       epost <- paste(url, "epost.fcgi?&db=pubmed&id=", paste(term, collapse=","),sep="" )
       gp <- readLines(epost)
   } else{ 
       if(length(term) >1) term  <- paste(term, collapse = " OR ")
       esearch <- paste(url, "esearch.fcgi?db=pubmed&retmax=1&usehistory=y&term=",  gsub(" ", "%20", term ) , sep = "")
       gp <- readLines(esearch) 
   }
   # CHECK FOR errors
   connecterror <- grepl("ERROR", gp)
   if (any(connecterror)) {
        # R CMD check on bioconductor may fail if using stop() 
        print(paste("Error connecting to NCBI ", gp[connecterror]))
   #-----------------------------------------------------------------------
   # PARSE Query key and web env
   }else{
      # EPost returns query key in line 4 and web env in line 5
      if (length(term) > 1) {
          x<-gsub("<[^>]*>", "", gp)
          x <- gsub("\t", "", x)
      }else{
           # Esearch returns all results on line 3.  Parse into 5 fields: count, retmax, retstart, querykey, webenv
           x <- unlist( strsplit( gp[3], "<[^>]*>" ))
           x <- x[x != ""]
           if (!x[1] > 0) { stop("No matches to ", term, " found") }
      }
      #-------------------------------------------------------------------

      efetch <- paste(url, "efetch.fcgi?db=pubmed&retmode=xml&retmax=1000&query_key=", 
              x[4], "&WebEnv=", x[5], sep = "")
      gp  <- readLines(efetch) 
      doc <- xmlParse(gp)  
      z<- getNodeSet(doc, "//PubmedArticle")
      n<-length(z)
      if(n==0){stop("No results found")} 
      pubs<-vector("list",n)
      for(i in 1:n)
      {
         # use xmlDoc or memory leak -see ?getNodeSet for queries on subtree..
         z2<-xmlDoc(z[[i]])

         pmid    <- as.numeric(xvalue(z2, "//PMID"))  # first PMID id
        # author  <- xvalue(z2, "//Author/LastName")  # first author
        
       a1<-xpathSApply(z2, "//Author/LastName", xmlValue)
       a2<-xpathSApply(z2, "//Author/Initials", xmlValue)
       a3<-paste(a1,a2)  # always same length? if not recycles.

        if(length(a3)>3){authors<-paste(c(a3[1:3], "et al."),  collapse=", ")}
        else{authors<-paste(a3,  collapse=", ")}

         year    <- as.numeric(xvalue(z2, "//PubDate/Year"))
         title   <- xvalue(z2, "//ArticleTitle")
         title   <- gsub("\\.$", "", title)
         journal <- xvalue(z2, "//Title")
         volume  <- xvalue(z2, "//Volume")
         pages   <- xvalue(z2, "//Pagination")
  
         pubdate <- xvalue(z2, "//PubDate")
         artdate <- xvalue(z2, "//ArticleDate")
         # lot of other dates in <History> like submitted, accepted, ahead of print (same as article date)
         # aopdate <- xvalue(z2, '//PubMedPubDate[@PubStatus="aheadofprint"]')

         pubs[[i]]<-data.frame(pmid, authors, year, title, journal, volume, pages, pubdate, artdate,
         stringsAsFactors=FALSE)
    
          free(z2)
      }
      do.call("rbind", pubs)
   }
}
