elink <-function(id, cmd="neighbor_history", parse=TRUE, ...)
{  
   email <- Sys.getenv("email")  
   if(email == ""){print("WARNING: please set your email using Sys.setenv(email='name@email.com')" ) }
   # ID can be a comma-seapated list of ids or the default results from esearch
   if(class(id)[1] == "EntrezHistory"){
        opts <- c(dbrom=id$db, query_key = id$query_key, WebEnv = id$WebEnv)
   }else{
        id <- gsub(" ", "", id) # remove spaces
        if(is.vector(id)) id<-paste(id, collapse=",")
        opts <- c( id=id)
   }
   opts <- c(email=email, tool="elink.R", opts, cmd=cmd, ...)
   opts <- paste( paste(names(opts), opts, sep="="), collapse="&")
   if( any(duplicated(names(opts)))){ stop("Duplicated keys are not allowed in url strings")}
   link <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi"   
   link <- paste(link, opts, sep = "?")
   gp <-xmlParse( readLines(link) )
   # parse the output
   if(parse){
      # xvalue in genomes package 
      if( !is.na( xvalue(gp, "//ERROR") )){
         stop( xvalue(gp, "//ERROR") )
      }
      # return history object 
      if(cmd == "neighbor_history"){         
         query <- xvalue(gp, "//QueryKey")
         if(is.na(query)){ 
             stop( "No linked results found")
         }
         web <- xvalue(gp, "//WebEnv")
         dbto <- xvalue(gp, "//DbTo")
         linkname <- xvalue(gp, "//LinkName")
         y <- data.frame( db= dbto, link=linkname, query_key = query, WebEnv = web, stringsAsFactors=FALSE)
         class(y) <- c("EntrezHistory", "data.frame")
         y
      }else{
         # or neighbor id list
         if(cmd == "neighbor"){
            ids<- xpathSApply(gp, "//Link/Id", xmlValue)
            # print(paste(length(ids), "linked records"))
            # paste(ids, collapse=",") # comma-seaprated?
            as.numeric( ids)
         }else{ 
            gp
         }
      }
   # or return XML
   }else{
      gp
   } 
}
