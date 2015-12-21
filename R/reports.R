reports <- function( file , assembly = FALSE ){
   url <- "ftp://ftp.ncbi.nih.gov/genomes/"
   dir <- "GENOME_REPORTS/"
   if(assembly ) dir <- "ASSEMBLY_REPORTS/"
   if(missing(file)){
      url <- paste0(url, dir)
      message("Listing files in ", url)
      con <- curl( url )
      x <- readLines(con)
      close(con)
   }else{
      url <- paste0(url, dir, file)
      message("Downloading ", url)
      x <- read_delim( url, "\t", na="-", quote="")
      # replace # in 1st column name
      names(x)[1] <- gsub("# *", "", names(x)[1])
      # easier for indexing and subsets 
      names(x) <- gsub("Organism/Name", "Organism", names(x))

      attr(x, "url") <- url
      attr(x, "date") <- date
      message(nrow(x), " rows and ", ncol(x), " columns")
   }
   x
}
