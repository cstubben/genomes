
## parse XML from ENA

# see http://www.ebi.ac.uk/ena/about/sra_preparing_metadata


enaParse<-function( doc ){
   portal <- attr(doc, "portal")
   if(is.null(portal)){stop("Cannot parse XML without portal attribute describing record type") }

# SAMPLES
   if(portal == "sample"){
      z <- getNodeSet(doc, "//SAMPLE")
      n <- length(z)
      if(n==0) stop("Cannot parse sample tags") 
      sra <- vector("list", n)
      for (i in 1:n) {
         z2 <- xmlDoc(z[[i]])
         taxid <- as.numeric( xvalue(z2, "//TAXON_ID") )
         name <- xvalue(z2, "//SCIENTIFIC_NAME")
         title <- xvalue(z2, "//TITLE")
         title <- gsub("([^;]*).*", "\\1", title)   # multiple titles ?
         # use title if scientific name is missing or keep separate?
         if(is.na(name)) name <- title
         sample  <- xattr(z2, "//SAMPLE", "accession")   ## same as PRIMARY_ID
         alias   <- xattr(z2, "//SAMPLE", "alias")       ## same as SUBMITTER_ID
         alias <- gsub(name, "", alias)  # alias is often duplicate of name
         center <- xattr(z2, "//SAMPLE", "center_name")
         center <- gsub("([^,]*).*", "\\1",center)    # multiple centers?
        description <- xvalue(z2, "//DESCRIPTION")

         biosample      <- xtags(z2, "//ENTREZ_LINK", "DB", "ID", "biosample")
         study      <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-STUDY")
         submission <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-SUBMISSION")
         experiment <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-EXPERIMENT")
         #run       <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-RUN")
         bases  <- as.numeric( xtags(z2, "//SAMPLE_ATTRIBUTE", "TAG", "VALUE", "ENA-BASE-COUNT") )
         reads  <- as.numeric( xtags(z2, "//SAMPLE_ATTRIBUTE", "TAG", "VALUE", "ENA-SPOT-COUNT") )
         sra[[i]] <- data.frame(sample, name, alias, taxid, submission, study, experiment, biosample, bases, reads, description, center, stringsAsFactors = FALSE)
         free(z2)
      }
      sra <- do.call("rbind", sra)
      sra <- sra[order(sra$name, sra$alias),]
      rownames(sra) <- NULL
      sra
# STUDIES
   }else if(portal == "study"){
      z <- getNodeSet(doc, "//PROJECT") 
      n <- length(z)
      if(n==0) stop("Cannot parse project tags") 

      prj <- vector("list", n)
      for (i in 1:n) {
         z2  <- xmlDoc(z[[i]])

        ## bioproject ID - use bioproject ACC instead
        ##  bioproject <- xtags(z2, "//PROJECT_ATTRIBUTE", "TAG", "VALUE" ,"PROJECT-ID")
         ## SUBMISSION _PROJECT
         taxid     <- as.numeric(xvalue(z2, "//TAXON_ID"))
         name      <- xvalue(z2, "//SCIENTIFIC_NAME")
         locus     <- xvalue(z2, "//LOCUS_TAG_PREFIX")
         scope     <- xattr(z2, "//SUBMISSION_PROJECT", "scope")
         material  <- xattr(z2, "//SUBMISSION_PROJECT", "material")    
         selection <- xattr(z2, "//SUBMISSION_PROJECT", "selection")    
         ## PROJECT
        #  project     <- xvalue(z2, "//PROJECT/NAME")      # same as scientific name?
         title       <- xvalue(z2, "//PROJECT/TITLE")
         description <- xvalue(z2, "//PROJECT/DESCRIPTION")
         description <- gsub("<[^>]*>", "", description)  # remove html tags 
         project  <- xattr(z2, "//PROJECT", "accession")
         released    <- xattr(z2, "//PROJECT", "first_public")
          if(released=="") released<-NA
         released    <- as.Date(substr(released, 1, 10))
         center      <- xattr(z2, "//PROJECT", "center_name")

         prj[[i]] <- data.frame(project, name, released, locus, 
                scope, material, selection, taxid, 
                 title, center, description, stringsAsFactors = FALSE)
         free(z2)
      }
      prj <- do.call("rbind", prj)
      prj <- prj[order(prj$name ), ]
      rownames(prj) <- NULL
       # class(prj)<-c("genomes", "data.frame")
       ## save date 
       attr(prj, "date")   <- Sys.Date()
      prj
# READ STUDIES
   }else if(portal == "read_study"){
      z <- getNodeSet(doc, "//STUDY")
      n <- length(z)
      if(n == 0) stop("Cannot parse study tags") 
      stdy <- vector("list", n)
      for (i in 1:n) {
         z2 <- xmlDoc(z[[i]])

         study <- xattr(z2, "//STUDY", "accession")       ## same as PRIMARY_ID
         # broker <- xattr(z2, "//STUDY", "alias")        ## same as SUBMITTER_ID, similar to TITLE
         # broker <- xattr(z2, "//STUDY", "broker_name")
         center <- xattr(z2, "//STUDY", "center_name")

         title <- xvalue(z2, "//STUDY_TITLE")
         type  <- xattr(z2, "//STUDY_TYPE", "existing_study_type")

         #xvalue(z2, "CENTER_PROJECT_NAME")  # same as title
        abstract <- xvalue(z2, "//STUDY_ABSTRACT") 

         project <-  xtags(z2, "//RELATED_LINK", "DB", "LABEL", "bioproject") 
         
         # samples, experiment, run, submission, and anlysis
         sample <-  xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-SAMPLE") 
         ## analysis<- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-ANALYSIS") 

         bases  <- as.numeric( xtags(z2, "//STUDY_ATTRIBUTE", "TAG", "VALUE", "ENA-BASE-COUNT") )
         reads  <- as.numeric( xtags(z2, "//STUDY_ATTRIBUTE", "TAG", "VALUE", "ENA-SPOT-COUNT") )

         stdy[[i]] <- data.frame(study, title, type, project, center,  sample,  bases, reads, abstract, stringsAsFactors = FALSE)
         free(z2)
      }

      stdy <- do.call("rbind", stdy)
      stdy <- stdy[order(stdy$title ), ]
      rownames(stdy) <- NULL
      ## save date 
       attr(stdy, "date")   <- Sys.Date()
      stdy
# READ EXPERIMENTS
   }else if(portal == "read_experiment"){
      z <- getNodeSet(doc, "//EXPERIMENT")
      n <- length(z)
      if(n == 0) stop("No read experiments found")  
      exp <- vector("list", n)
      for (i in 1:n) {
         z2 <- xmlDoc(z[[i]])

         experiment <- xattr(z2, "//EXPERIMENT", "accession")     ## same as PRIMARY_ID
         submission <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-SUBMISSION")
         title     <- xvalue(z2, "//TITLE")
         description <- xvalue(z2, "//DESIGN_DESCRIPTION")  #

         # PLATFORM
         platform  <- xpathSApply(z2, "//PLATFORM/*", xmlName)
         model     <- xvalue(z2, "//INSTRUMENT_MODEL")
         # LIBRARY
         library      <- xvalue(z2, "//LIBRARY_NAME")
         # strategy  <-xvalue(z2, "//LIBRARY_STRATEGY") # all wgs?
         protocol <- xvalue(z2, "//LIBRARY_CONSTRUCTION_PROTOCOL")

         layout    <- xpathSApply(z2, "//LIBRARY_LAYOUT/*", xmlName)
         source    <- xvalue(z2, "//LIBRARY_SOURCE")
         selection <- xvalue(z2, "//LIBRARY_SELECTION")
         bases  <- as.numeric( xtags(z2, "//EXPERIMENT_ATTRIBUTE", "TAG", "VALUE", "ENA-BASE-COUNT") )
         reads  <- as.numeric( xtags(z2, "//EXPERIMENT_ATTRIBUTE", "TAG", "VALUE", "ENA-SPOT-COUNT") )

         exp[[i]] <- data.frame(experiment, title, platform, model, layout, source, selection, library, submission, bases, reads, description, protocol, stringsAsFactors = FALSE)
         free(z2)
      }
      exps <- do.call("rbind", exp)
      exps <- exps[order(exps$title ), ]
      rownames(exps) <- NULL
      ## save date 
      attr(exps, "date")   <- Sys.Date()
      exps
# SUBMISSIONS - NOT a portal - but can parse after searching for multiple submission ids
   }else if(portal == "submission"){
      ## NO LOOP 
      ## Feb 15, 2013  some with namespaces causes error using sapply
      y     <- xpathApply(doc, "//SUBMISSION", xmlAttrs, addNamespaceURLs=FALSE)
      title <- xpathSApply(doc, "//TITLE", xmlValue)
      submission <- as.vector( sapply(y, "[", "accession") )
      center <-     as.vector( sapply(y, "[", "center_name") )
      date <-  as.Date(substring( sapply(y, "[", "submission_date"), 1,10))
      submit  <- data.frame(submission, title, center, date,  stringsAsFactors=FALSE)
      attr(submit, "date")   <- Sys.Date()
      submit

   }else{
      print(paste("No parser for", portal, "available"))
   }


}
