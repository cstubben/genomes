esummary <-function(id, db="pubmed", parse=TRUE, ...)
{  
   email <- Sys.getenv("email")  
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   # ID can be a comma-seapated list of ids or the default results from esearch
   if(class(id)[1]=="EntrezHistory"){
      # is db always the same as ESearch db?
      opts <- c(db=id$db, query_key = id$query_key, WebEnv = id$WebEnv)
   }else{
      id <- gsub(" ", "", id) # remove spaces
      if(is.vector(id)) id<-paste(id, collapse=",")
      opts <- c(id=id, db=db)
   }
   opts <- c(email=email, tool="esummary.R", opts, ...)
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}

   esum <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"   
   esum <- paste(esum, opts, sep = "?")
   gp <- xmlParse(readLines(esum))
   if( !is.na( xvalue(gp, "//ERROR") )){
         xvalue(gp, "//ERROR")
  }else{
   if(parse){
      # version 2.0 uses DocumentSummary tag
      version2 <- is.na( xvalue(gp, "//DocSum"))
      if( version2){
          z <- getNodeSet(gp, "//DocumentSummary")
      }else{
          z <- getNodeSet(gp, "//DocSum")
          # replace Item columns 
          columns <- as.vector(unlist(lapply(xmlApply(z[[1]], xmlAttrs), '[[', 1)))
      }
      x <- lapply(z, function(x) xmlSApply(x, xmlValue))
      ## check if all rows have same number of tags (some are missing like DOI tags in some pubmed records)
      if( length( unique( sapply(x, length))) > 1 ){
          print("WARNING: some records are missing tags and therefore rows in the table will be shifted")
          # find messed up rows with one missing tag using .... subset(x, x[1]== x[ncol(x)]) since values are recycled
      }
      x <- do.call("rbind", x)
      x <- data.frame(x, stringsAsFactors = FALSE)
      if(!version2){  
         if(ncol(x)-1 == length(columns)) colnames(x)[-1] <- columns
      }
      x
   }else{
      gp
   }
  }
}
