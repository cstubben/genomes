ncbiInfo<-function(db, list="field")
{
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/einfo.fcgi"
   if(missing(db)){
     x <- readLines( url  )
     doc <-  xmlParse(x) 
     x <- xpathSApply(doc, "//DbName", xmlValue)
     data.frame(DbName=sort(x) ) 
  }else{
     x <- readLines( paste(url, "?db=", db, sep="") )
     if( grepl("ERROR", x[4])){
        print(paste("Can not retrieve DbInfo for db=", db, sep=""))
     }else{
        doc <-  xmlParse(x) 
        # Field List or Link List...
        if( list=="field"){
          z <- getNodeSet(doc, "//FieldList/Field")
        } else{
          z <- getNodeSet(doc, "//LinkList/Link")
        }
        x <- sapply(z, function(x) xmlSApply(x, xmlValue))
        x <- data.frame(t(x), stringsAsFactors = FALSE)
        x <- x[order(x[,1]),]
        rownames(x) <- NULL
        #names(x) <- tolower(names(x))
        x
     }
  }
}
