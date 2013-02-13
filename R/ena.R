## display = xml, text or fasta


ena <- function( ids, portal, subtree=TRUE, limit =1000, display="xml")
{ 
   if(missing(portal)){
      if(length(ids)>1){
          if(any(is.na(ids))){ids <- ids[!is.na(ids)] }
          if(any(duplicated(ids))){ ids <- unique(ids) }
          if(length(ids) > 100){
               ids <- ids[1:100]
               print("Warning: only first 100 ids used")
          }
          ids <- paste(ids, collapse = ",")
      }
      ## check if taxonomy id - and use portal!
  

      url1 <-"http://www.ebi.ac.uk/ena/data/view/"
      url  <- paste(url1,  ids, "&display=", display, sep="")
      print(paste("Downloading", url))
   
       ## DNA only?
      if(display == "fasta"){
         readDNAStringSet(url)
      }else{
         x <- readLines(url, encoding="latin1")
    
        ## added Feb 5, 2013 - urls may be empty
         if(length(x) == 0){
            print("No results found")
         } else if(display == "xml"){
            doc <- xmlParse(x)
            doc
         }else{
            x
         }
      }
   # TAXONOMY portal options
   }else{
      if( length(ids) > 1){ ids <- ids[1]; print("Warning: only the first id will be used")}
      #check if number passed as character, eg  2  = "2"
      if(!is.na(suppressWarnings(as.numeric( ids)))){ ids <-as.numeric(ids) }
      if(!is.numeric(ids)){ ids <- tax2id(ids)  } 
       ## STATS
      if(portal=="stats"){

         url <- paste("http://www.ebi.ac.uk/ena/data/stats/taxonomy", ids, sep="/")
         x   <- read.table(url, row.names=1)
         colnames(x) <- c("direct",  "size" , "subtree" ,  "subsize")
         x
      }else{
         url1 <- "http://www.ebi.ac.uk/ena/data/view/Taxon:"
         url <- paste( url1, ids, "&portal=", portal, "&limit=", limit, "&display=", display, sep="")
         if(subtree) url<- paste(url, "&subtree=true", sep="")

         print(paste("Downloading", url))

         if(display == "fasta"){
            readDNAStringSet(url)
         }else{
            x <- readLines(url, encoding="latin1")
 
            if(length(x)==0){
               print("No results found")
            } else if(display == "xml"){
               ##Avoid error : Extra content at the end of the document
               x <-c("<ENA>", x, "</ENA>")
               doc <- xmlParse(x)
               attr(doc, "portal") <- portal
               doc
            ## text, fastq?
            }else{
               x
            }
         }
      }
   }
}
