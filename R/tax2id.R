tax2id <- function( name )
{
  id <- esearch(name, db="taxonomy", usehistory="n", verbose=FALSE)
  if(id[1]=="No results found."){ 
      stop(id) 
  }else{
      if(length(id) > 1)  print(paste("Warning:", length(id), "ids matching taxonomy name"))
      id[1]
   }                   
}
