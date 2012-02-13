enaProject <-function(tax, limit =1000, refseq=TRUE)
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
      z <- getNodeSet(doc, "//PROJECT")
      n <- length(z)
      print(paste( "Matches", n, "projects"))
      prj <- vector("list", n)
      for (i in 1:n) {
         z2  <- xmlDoc(z[[i]])

         pid <- xtags(z2, "//PROJECT_ATTRIBUTE", "TAG", "VALUE" ,"PROJECT-ID")
         ## SUBMISSION _PROJECT
         taxid     <- as.numeric(xvalue(z2, "//TAXON_ID"))
         name      <- xvalue(z2, "//SCIENTIFIC_NAME")
         locus     <- xvalue(z2, "//LOCUS_TAG_PREFIX")
         scope     <- xattr(z2, "//SUBMISSION_PROJECT", "scope")
         material  <- xattr(z2, "//SUBMISSION_PROJECT", "material")    
         selection <- xattr(z2, "//SUBMISSION_PROJECT", "selection")    
         ## PROJECT
         project     <- xvalue(z2, "//PROJECT/NAME")      # same as scientific name?
         title       <- xvalue(z2, "//PROJECT/TITLE")
         description <- xvalue(z2, "//PROJECT/DESCRIPTION")
         description <- gsub("<[^>]*>", "", description)  # remove html tags 
         accession   <- xattr(z2, "//PROJECT", "accession")
         released    <- xattr(z2, "//PROJECT", "first_public")
          if(released=="") released<-NA
         released    <- as.Date(substr(released, 1, 10))
         center      <- xattr(z2, "//PROJECT", "center_name")

         prj[[i]] <- data.frame(pid, name, released, locus, 
                scope, material, selection, taxid, 
                project, title, accession, center, description, stringsAsFactors = FALSE)
         free(z2)
      }
      prj <- do.call("rbind", prj)
      prj <- prj[order(prj$name ), ]
      rownames(prj) <- NULL
       class(prj)<-c("genomes", "data.frame")
       ## save date 
       attr(prj, "date")   <- Sys.Date()
      prj
   }  
}
