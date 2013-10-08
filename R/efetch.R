# MAr 26 - drop default seq_stop=700
# TO DO: add options to readDNAStringSet directly from url

efetch <-function(id, db="pubmed", rettype="", retmode="text", showURL=FALSE, destfile, ...)
{  
   email <- Sys.getenv("email")  
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   # ID can be a comma-separated list of ids or the default results from esearch
   if(class(id)[1]=="EntrezHistory"){
      # is db always the same as ESearch db?
      opts<-c(db=id$db, query_key = id$query_key, WebEnv = id$WebEnv)
      
   }else{
      id <- gsub(" ", "", id) # remove spaces
      if(is.vector(id)) id<-paste(id, collapse=",")
      opts<-c( id=id, db=db)
   }
   opts <- c(email=email, tool="efetch.R", opts, rettype=rettype, retmode=retmode, ...)
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")  
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}
   fetch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
   fetch <- paste(fetch, opts, sep = "?")
   if(showURL) print(fetch)

    # if retmode=xml-  will complain about  incomplete final line - use getURL 
   if(missing(destfile)){
      gp <- readLines(fetch)
      gp
   }else{
      download.file(fetch, destfile)
   }
}
