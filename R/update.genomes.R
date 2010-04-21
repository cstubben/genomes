update.genomes <- function(object, ...)
{
   ## Don't know how to update subsets of genome tables.
   ## For now, just return an error message
   if(!is.null(attr(object, "subsetof"))){
     stop("No update attribute found. ", sQuote(substitute(object)), " is a subset of ", attr(object, "subsetof"), ".")
   }
    ## if update attribute removed?
    if(is.null(attr(object, "update"))){
     stop("No update attribute found. ")
   }
   
   if(Sys.Date() == attributes(object)$date){
   stop("Genome projects already downloaded today", call.=FALSE) }

   ## Update using text string in update attribute
   prj <- eval(parse(text=attr(object, "update"))) 
   message(substitute(object), " has been successfully updated")

   ## STATISTICS
   ## column 1 should be unique and match old project table (add checks?) 
   id <- names(prj)[1]

   #if(  names(prj)[1] == names(object)[1]
   
   if(id=="pid") id<- "project IDs"
   if(id=="acc") id <- "accession numbers"
   if(id=="goldstamp") id <- "goldstamp IDs"
   if(id=="name") id <- "genomes"    ## virus table only has names

   # new projects added
   n <- (!prj[,1] %in% object[,1])
   if(sum(n)==1) id<-gsub("s$", "", id)
   message(sum(n), " new ", id, " added")

    ## OLD projects removed
    n <- (!object[,1] %in% prj[,1])
    if(sum(n)==1) id<-gsub("s$", "", id)
    if(sum(n) > 0){
      message(sum(n), " old ", id, " removed")
        
    }
    
   ## replace old object with new prj
   eval(eval(substitute(expression(object <<- prj)))) 
}
