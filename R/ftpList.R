ftpList<-function(ftp, fileonly = FALSE)
{
  # ftp starts with ftp://
  if(!grepl("^ftp://", ftp)){ftp <- paste("ftp://", ftp, sep="")}
  # to avoid warnings, ftp should end in "/"
  if(!grepl("/$", ftp)){ftp <- paste(ftp, "/", sep="")}
  x  <- try( getURL(ftp, ftp.use.epsv = FALSE), silent=TRUE)
  if(class(x) == "try-error"){stop("No directory found matching ", ftp)   }
  zz <- textConnection(x)
  # in case spaces are in FILE names
  x2 <- read.table(zz, as.is = TRUE, fill=TRUE)
  close(zz)

  n <- ncol(x2)
  if(n > 9){
     x9 <- apply(x2[,9:n], 1, paste, collapse=" ")
     x2[,9] <- gsub(" *$", "", x9)
     x2 <- x2[, 1:9]
  }
  colnames(x2)[c(5,9)] <- c("size", "name")
  x2$mode <- substr(x2[,1], 1,1)
  
  year <- format( Sys.Date(), "%Y")
  # ADD dates... current year has time 
  x2$date <- as.Date( paste( x2[,6], x2[,7], ifelse( grepl(":", x2[,8]), year, x2[,8])), "%b %d %Y")
  x2 <- x2[, c(9,10,5, 11)]

  if(fileonly){
       subset(x2, mode == "-", c(1,3,4))
    }else{
      subset(x2, mode != "l")  # skip links?
   }
}

