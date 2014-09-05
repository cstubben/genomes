read.gff <- function(file,  locus.tags=TRUE, nrows = -1  ){

   # columns 1 and 2 are saved in metadata (full seqid with version# and source )
  #  columns 4 = start , 5 = end and 7 = strand are saved in GRange

   x <- read.delim(file, stringsAsFactors=FALSE, comment.char="#", header=FALSE, nrows=nrows)
   # use tags instead of attributes() function
   colnames(x) <- c("seqid", "source", "feature", "start", "end", "score", "strand", "phase", "tags")

   # sep 4 , 2014 - temp fix for new FTP site with "." in strand...
   n<- x$strand=="."
   if(any(n)){
     x$strand[n] <- "+"
     print("WARNING: changing '.' to '+' in strand column")
   }

   # SAVE OR remove version number?   - strsplit2 in limma
   seqid  <- unique(x$seqid)
   if( all(grepl("\\.[0-9]$", seqid )) )  x$seqid<- genomes::strsplit2(x$seqid, ".", fixed=TRUE)


   ## extra ID tag added for plotting 
   x$id <-  gsub("ID=([^;]*).*", "\\1", x$tags)   #  -always at start of tags?

   ## FULL table (except score and phase)
   if(!locus.tags){
      gff <- GRanges(seqnames=x$seqid, ranges=IRanges( x$start, x$end), strand=x$strand, data.frame(x[, c(10, 3, 6,8, 9 ) ])  )

   }else{

      # SPLIT - using GENE or locus_tag key?  
      n <- x$tags %like% '*locus_tag=*'
      genes <- subset(x, n)
      y <-  subset(x, !n)

      ##FIX: gffs from IMG with exit with error since no rows in y (all locus tags)

      # get Parent ids for matching
      y$parent<-NA
      n <- y$tags %like% '*Parent=*'
      y$parent[n] <- gsub(".*Parent=([^;]*).*", "\\1", y$tags[n])

     # get protein_ids (for matching to fasta and other files)   ## ADDED Dec 23, 2013
     y$pid <- ""
      n<- y$tags %like% '*protein_id=*'
      if(sum(n)>0) y$pid[n]  <-  gsub(".*protein_id=([^;.]*).*", "\\1", y$tags[n])

      # get Products 
      y$product <- ""
      n<- y$tags %like% '*product=*'
      if(sum(n)>0) y$product[n]  <-  gsub(".*product=([^;]*).*", "\\1", y$tags[n])
    
      ## add notes if missing product (for ncRNAs and  transposase fragments)
      n<- is.na(y$product) & y$tags %like% '*Note=*'
      y$product[n]  <-  gsub(".*Note=([^;]*).*", "\\1", y$tags[n])

      n   <- grep("%", y$product)
      if(length(n)>0) y$product[n] <- as.vector(sapply(y$product[n] , URLdecode))
   
      ## ADD locus tags  
      # Mar 2013 Mycoplasma has old_locus_tag, add semi-colon    ;locus_tag=MG_515;old_locus_tag=MG323.1
      genes$locus <- gsub(".*;locus_tag=([^;]*).*", "\\1", genes$tags)
   
      ## FIND protein coding and other genes types USING parent key
       n <- match(  genes$id, y$parent )  
      n2 <- !is.na(n) 
      genes$feature[n2] <-  y$feature[n[n2]]   # overwrite pseudo tags
      
       genes$description <- ""
      genes$description[n2] <-  y$product[n[n2]]
      genes$pid[n2]         <-  y$pid[n[n2]]

       ###   FIX pseudogenes
      genes$feature[ genes$tags %like% '*pseudo=true*'] <- "pseudo"

      ## GENES
      n <- which(genes$feature =="gene" )

      ## genes can match to region (other) and tRNAs
      if(length(n) > 0){
         n2 <- match(  paste(genes$start[n], genes$end[n]), paste(y$start, y$end)  )
         # check for matches. - and set type = other below
          n3 <- !is.na(n2) 

         genes$feature[n[n3]] <-  y$feature[n2[n3]]
         genes$description[n[n3]] <-  y$product[n2[n3]] 
      }

      ## Y pestis pCD1 has 'transcript'  = miscRNA and region = other AT NCBI  
      genes$feature [genes$feature == "transcript" ] <-  "miscRNA"
       genes$feature [genes$feature == "region" ] <-  "other"
       genes$feature [genes$feature == "gene" ] <-  "other"   


      ## get gene AND gene_synonym
      genes$gene <- ""
      n<-grep("gene=", genes$tag)
      if(length(n)>0)  genes$gene[n] <- gsub(".*gene=([^;]*).*", "\\1", genes$tags[n])

      n<-grep("gene_synonym=", genes$tag)
      if(length(n)>0)  genes$gene[n] <- paste( genes$gene[n] ,  gsub(".*gene_synonym=([^;]*).*", "\\1", genes$tags[n]) , sep=",")

      n   <- grep("%", genes$gene)
      # if no matches, avoid changing genes to list() 
      if(length(n)>0)  genes$gene[n] <- as.vector(sapply(genes$gene[n] , URLdecode))

      ## check duplicate tags (combined using JOINs)

      if( any(table(genes$locus)>1) ){
         # or create GRange and then reduce()
         print("Warning: grouping coordinates for duplicate locus tags") 
         genes2 <- by(genes, genes$locus, function(x){
             data.frame(locus=x$locus[1], seqid=x$seqid[1], start=min(x$start), end = max(x$end), strand= x$strand[1], 
                        pid=x$pid[1], feature=x$feature[1], description=x$description[1], gene=x$gene[1] ) 
                   })
         genes <- do.call('rbind', genes2) 

      } 

      # re-order ?
      if( any(diff(order(genes$start))!=1) ){
         genes <-genes[order(genes$start),] 
      }
      gff <- GRanges(seqnames=genes$seqid, ranges=IRanges( genes$start, genes$end), strand=genes$strand, genes[, c("locus", "pid", "feature", "description", "gene") ]  )
   }
   ##  sequence length should be in first row (and max end) 
   ## gff may have multiple seq ids (then find if "source" tag present
   if(length(seqid)==1)  seqlengths(gff) <- max(x$end)
   # add defline for read.gff - parse from source tag???  - use full acc with version number
   metadata(gff) <- list(source=unique(x$source) , defline=seqid)
   gff
}



