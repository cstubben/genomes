esearch <-function(term, db="pubmed", usehistory="y", parse=TRUE, verbose=TRUE, showURL=FALSE, ...)
{
    # or use options("verbose")
   if(is.vector(term)) term <- paste(term, collapse=",")   # collapse=" OR " ?
   term  <- gsub(" ", "+", term)  # replace spaces
   email <- Sys.getenv("email")  # will be empty string if not set
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   opts <- c(email=email, tool="esearch.R", db=db, term=term, usehistory=usehistory, ...)
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")

   search <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
   search <- paste(search, opts, sep = "?")
   
   if(showURL) print(search)

   gp <-xmlParse( readLines(search) )
   # parse the output
   if(parse){
      # xvalue in genomes package 
      if( !is.na( xvalue(gp, "//ERROR") )){
         xvalue(gp, "//ERROR")
      }else{
         count <- as.numeric(xvalue(gp, "//Count"))
         if(count == 0){ 
            "No results found."
            # xvalue(gp, "//OutputMessage")  #OR
         }else{
            # return history object for esummary or efetch
            if(usehistory == "y"){
              if(verbose){ 
                 if(count == 1){ print("1 result found") 
                 }else{         print(paste( count, "results found")) }
              }
                query <- xvalue(gp, "//QueryKey")
                web   <- xvalue(gp, "//WebEnv")
                y <- data.frame( db= db, results=count,  query_key = query, WebEnv = web, stringsAsFactors=FALSE)
                class(y) <- c("EntrezHistory", "data.frame")
                y
            # or id list
            }else{
                retmax <- xvalue(gp, "//RetMax")
                if(verbose){
                  if(retmax == count){
                    if(count >1 ) print(paste( retmax, db, "ids returned"))
                  }else{          print(paste( retmax, " ", db, " ids returned (", count, " total ids)", sep="")) }
                }
                # paste(xpathSApply(gp, "//Id", xmlValue), collapse=",")  # comma-separated?
                as.numeric( xpathSApply(gp, "//Id", xmlValue) )
            }
         } 
      }
   # or return XML
   }else{
      gp
   } 
}
