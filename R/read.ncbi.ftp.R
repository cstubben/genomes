read.ncbi.ftp <- function(org,  filePattern="ptt$|rnt$", ftp ="genomes/Bacteria", ...){
   ftpdir <- paste("ftp://ftp.ncbi.nih.gov", ftp, org, sep="/")
   # list files in directory
   x <- ftpList( ftpdir )
   files <- grep(filePattern, x$name, value=TRUE)
   if(length(files) == 0){stop("No files matching ", filePattern, " found")}
   # get file endings
   fileTypes <- strsplit2(files, ".", 2, fixed=TRUE)
    # CANNOT read these files
   n0 <- which( fileTypes %in% c("asn", "gbk", "val", "GeneMark-2", "rpt"))
   if(length(n0) > 0 ){
      files <- files[-n0] 
      if(length(files) == 0){stop("Cannot read files ending in ", unique(fileTypes) )}
      fileTypes <-  fileTypes[-n0]   
   }
   n <- length(files)
   z <- vector("list", n) 

   ## Coordinates 
   if(all( fileTypes %in% c("ptt", "rnt", "GeneMarkHMM-2", "Glimmer3", "Prodigal-2", "gff"))){
      for(i in 1:n){
         print(paste("Downloading", files[i]))
         file <- paste(ftpdir, files[i], sep="/")
 
         if(      fileTypes[i] == "GeneMarkHMM-2"){ 
            z[[i]] <- read.genemark( file, ... )
         }else if(fileTypes[i] == "Glimmer3") {
            z[[i]] <- read.glimmer( file, ... )
         }else if(fileTypes[i] == "Prodigal-2") {
            z[[i]] <- read.prodigal( file, ... )
         }else if(fileTypes[i] == "gff") {
            z[[i]] <- read.gff( file, ... )
         }else{
            z[[i]] <- read.ptt( file , ...)
            # ADD feature type?
            if (fileTypes[[i]] == "ptt") {
               values(z[[i]])$feature <- "CDS"
            }else{
               values(z[[i]])$feature <- "RNA"
            }
         }
      }
      ## COMBINE  into single object
      deflines <- unique( sapply(z, function(y) metadata(y)$defline))

      ## suppress warnings about combining different seq accessions
      z <- suppressWarnings( do.call("c", z) )

      #  Sort by chromosome size AND START 
      n <- order(seqlengths(z), decreasing=TRUE)
      seqlevels(z) <- seqlevels(z)[n]
     z <- z[order( match(IRanges::as.vector(seqnames(z)),  seqlevels(z)), start(z)), ]
      # also order deflines but not files because rnt and ptt possible
      if(length(n) == length(deflines)) deflines <- deflines[n]

      # add new metadata
      metadata(z) <- list(ftp = ftpdir, files=files,  defline=deflines, date=Sys.Date() )
   }


   ## AA sequences
   if(all(fileTypes %in% c("faa"))){
      for(i in 1:n){
         print(paste("Downloading", files[i]))
         file <- paste(ftpdir, files[i], sep="/")
         z[[i]] <- read.AAStringSet(file, ...)
      }
      z <- do.call("c", z) 
   }
   ## DNA sequences
   if(all(fileTypes %in% c("fna", "frn", "ffn"))){
      for(i in 1:n){
         print(paste("Downloading", files[i]))
         file <- paste(ftpdir, files[i], sep="/")
         z[[i]] <- read.DNAStringSet(file, ...)
      }
      z <- do.call("c", z) 
      ## check if single sequence (then use DNAString instead of DNAStringSet)
      if(length(z)==1) z <- z[[1]]
   }


   ## z is empty ? mixed file types???
   if(is.list(z) &&  all(sapply(z, is.null)) ) stop("Too many different file types: ", paste(unique(fileTypes), collapse=", "))
   z
}

