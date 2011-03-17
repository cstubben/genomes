acc2date<-function(ids, common=TRUE )
{
   if(is.null(ids)){ stop("Ids are null")}
   if(length(ids) > 1){ ids <- paste(ids, collapse=",")}
   if(grepl(" ", ids)){ ids <- gsub(" ", "", ids) }
 
   url <- "http://www.ncbi.nlm.nih.gov/sviewer/girevhist.cgi?val="

   xhist  <- readLines(paste(url, ids, sep=""))
   n1 <- grep("first seen", xhist)
   if(length(n1) == 0){   stop("No results found") }
   
   x2 <- gsub("(.*>)([^<]*)(<.*was first seen at NCBI on )(.*[[:digit:]]{4})(.*)", "\\2---\\4",   xhist[n1], perl=TRUE)
   x2 <- do.call("rbind", strsplit(x2, "---"))
   revhist <- data.frame(x2, stringsAsFactors=FALSE)
   colnames(revhist) <- c("id", "released")
   revhist$released  <- as.Date(revhist$released, format = "%b %d %Y")
   if(common)
   {
      revhist$common<-FALSE
      n2 <- grep("Common Rev", xhist)
      if(length(n2)>0)
      {      
         for(i in 1:length(n2))
         {
            j <- which(n1 > n2[i] )[1]  # which row in revhist to change?

            ids <- gsub("(.*girevhist.cgi\\?val=)(.*)(\">Common Rev.*)", "\\2",   xhist[n2[i]])
            # some common revision history links have > 500 IDs and return error
            # still get warning with silent?
            suppressWarnings( xcom <- try(readLines(paste(url, ids, sep="")),   silent=TRUE))
            
            if(class(xcom) == 'try-error'){
               revhist$released[j] <- NA
               revhist$common[j]   <- TRUE
            }else{
               x1 <- grep("first seen", xcom, value=TRUE)
               x2 <- gsub("(.*first seen at NCBI on )(.*[[:digit:]]{4})(.*)", "\\2",   x1, perl=TRUE)
               x2 <- min( as.Date(x2, format = "%b %d %Y"))   # minimum date
               revhist$released[j] <- x2
               revhist$common[j]   <- TRUE
            }
         }
      }
  }
  revhist
}
    
 
