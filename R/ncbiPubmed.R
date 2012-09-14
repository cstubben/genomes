ncbiPubmed<-function(term, abstract=FALSE)
{ 
   if(length(term) > 1){ term  <- paste(term, collapse = ",") }  

   # CHECK if IDs (and skip esearch)
   if( grepl("^[0-9, ]*$", term)){
     x <- efetch(term, "pubmed", retmode="xml")
   }else{
     x <- efetch(esearch(term, "pubmed") , retmode="xml" )
   }

      doc <- xmlParse(x)  
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
         ## full journal (with lower case )  OR abbrev
         # journal <- xvalue(z2, "//Title")
         journal <- xvalue(z2, "//ISOAbbreviation")
         volume  <- xvalue(z2, "//Volume")
         pages   <- xvalue(z2, "//Pagination")
  
         pubdate <- xvalue(z2, "//PubDate")
         artdate <- xvalue(z2, "//ArticleDate")
         # lot of other dates in <History> like submitted, accepted, ahead of print (same as article date)
         # aopdate <- xvalue(z2, '//PubMedPubDate[@PubStatus="aheadofprint"]')

    

        abstracttext <- xvalue(z2, "//AbstractText")

         pubs[[i]]<-data.frame(pmid, authors, year, title, journal, volume, pages, pubdate, artdate, abstracttext,
         stringsAsFactors=FALSE)
    
          free(z2)
      }
      x<- do.call("rbind", pubs)
      if(!abstract) x<- x[, -10]
      x
}




