pub2date<-function(pmids)
{
   if(is.null(pmids)){ stop("Pmids are null")}
   if(length(pmids)>1){ pmids<-paste(pmids, collapse=",")}
   if(grepl(" ", pmids)){ pmids<-gsub(" ", "", pmids) }
  
   url<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmax=500&retmode=xml&id="
 
   z<-paste(url, pmids, sep="") 
   x<-readLines(z)

   # if NO tag, return NA instead of empty list (also gets first value returned)
   xpath <- function(doc, q) {
      res <- xpathSApply(doc, q, xmlValue)
      if (length(res)>0) res[1]
      else NA_character_
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

         pmid    <- as.numeric(xpath(z2, "//PMID"))  # first PMID id
        # author  <- xpath(z2, "//Author/LastName")  # first author
        
       a1<-xpathSApply(z2, "//Author/LastName", xmlValue)
       a2<-xpathSApply(z2, "//Author/Initials", xmlValue)
       a3<-paste(a1,a2)  # always same length? if not recycles.

        if(length(a3)>3){authors<-paste(c(a3[1:3], "et al."),  collapse=", ")}
        else{authors<-paste(a3,  collapse=", ")}

         year    <- as.numeric(xpath(z2, "//PubDate/Year"))
         title   <- xpath(z2, "//ArticleTitle")
         journal <- xpath(z2, "//Title")
         volume  <- xpath(z2, "//Volume")
         pages   <- xpath(z2, "//Pagination")
  
         pubdate <- xpath(z2, "//PubDate")
         artdate <- xpath(z2, "//ArticleDate")
         # lot of other dates in <History> like submitted, accepted, ahead of print (same as article date)
         # aopdate <- xpath(z2, '//PubMedPubDate[@PubStatus="aheadofprint"]')

         pubs[[i]]<-data.frame(pmid, authors, year, title, journal, volume, pages, pubdate, artdate,
         stringsAsFactors=FALSE)
    
          free(z2)
      }
      do.call("rbind", pubs)
}
