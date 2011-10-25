enaProject <-function(tax, limit =1000, refseq=FALSE)
{
   if( length(tax) > 1){tax <- tax[1]; print("Warning: only the first id will be used")}
   #check if number passed as character, eg  2  = "2"
   if(!is.na(suppressWarnings(as.numeric( tax)))){ tax <-as.numeric(tax) }
   if(!is.numeric(tax)){ tax <- tax2id(tax)  }
   url1 <- "http://www.ebi.ac.uk/ena/data/view/Taxon:"
   url2 <- paste( "&portal=project&limit=", limit, "&subtree=true&display=xml", sep="")
   url  <- paste(url1, tax, url2, sep="")
   x <- readLines(url)
     connecterror <- grepl("ERROR", x)
     if (any(connecterror)) {
        print(paste("Error connecting to NCBI ", x[connecterror]))
     }
   # IF no result using display =xml , then 2 lines returned with request in root tag
   if(length(x) == 2 ){ print("Result not found.") 
   } else{
      doc <- xmlParse(x)
      ###   CREATE LOOP
      z <- getNodeSet(doc, "//Project")
      n <- length(z)
      prj <- vector("list", n)
      for (i in 1:n) {
         z2  <- xmlDoc(z[[i]])
         pid      <- as.numeric( xvalue(z2, "//ProjectID") )
         taxid      <- as.numeric( xvalue(z2, "//TaxID") )
         name     <- xvalue(z2, "//Submission/OrganismName")
         # strain <- xvalue(z2, "//Submission/Strain")
         locus      <- xvalue(z2, "//Submission/LTP")
         type     <- xvalue(z2, "//Submission/Type")
         method   <- xvalue(z2, "//Submission/Method")
         submitted    <- xvalue(z2, "//Submission/Date")
          if(!is.na(submitted)){ submitted <- as.Date(substr(submitted, 1,10), "%m/%d/%Y") }
         center <- xvalue(z2,  "//Submission/Submittor" )
         #center   <- xvalue(z2, "//Center/Name")
         model <- xvalue(z2, "//Submission/SeqTechno")
         depth<- xvalue(z2, "//Submission/SequenceDepth")
         annotated <- xvalue(z2, "//Submission/Annotated")
       
         habitat  <- xvalue(z2, "//Description/Habitat")
         pathogen <- xvalue(z2, "//Description/Pathogen")
         defline  <- xvalue(z2, "//DefLine")
         size     <- xvalue(z2, "//GenomeSize")         
         replicons<- xpathSApply(z2, "//Chromosomes/ChrType", xmlValue)
         chr      <-sum(replicons == "eChromosome")
         # plasmid  <-sum(replicons == "ePlasmid")
         orgnl      <-sum(replicons %in% c("ePlasmid", "eMitochondrial") )
         wgs      <-sum(replicons == "eWGS")


           ## check replicons types?
         if(length(replicons) != chr+orgnl+wgs){print(paste("WARNING: new ChrType:", unique(replicons)))}
         # remove "e"
         type <- gsub("^e", "", type)
          method <- gsub("^e", "", method)
          model <- gsub("^e", "", model)


         prj[[i]] <- data.frame(pid, taxid, name, defline, submitted,   type, method, model, depth, annotated, center, locus, habitat, pathogen, size, wgs, chr, orgnl, stringsAsFactors = FALSE)
         free(z2)
      }
      prj <- do.call("rbind", prj)
      if(!refseq) { prj <- subset(prj, type!="RefSeq")}
      prj <- prj[order(prj$name, prj$type),]
      rownames(prj) <- NULL
      prj
   }  
}
