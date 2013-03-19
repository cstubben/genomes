                                                 
## FIND characters to fix

# USE loop to check every column?  see old updatelproks
## isLatin1 <- apply(prj, 2, function(y) any( grepl("NOT ASCII", iconv(y, "latin1","ASCII", sub="NOT ASCII"))))

## OR just center
#  x <- unique(c(proks$center, euks$center) )
#  n <- is.na(iconv(x, "latin1", "ASCII"))
#  y <- iconv(x[n], "latin1", "ASCII", sub="byte")
#  z <-sort(gsub("--", "><", unique(unlist(str_extract_all(gsub("><", "--",y),  "<[^>]*>")))))

#  cat(noquote( paste('   x[n] <- gsub("', z, '", "", x[n])', sep="")), sep="\n")


latin2char<-function(x)
{
   n <- is.na(iconv(x, "latin1", "ASCII"))
   x[n] <- iconv(x[n], "latin1", "ASCII", sub="byte")


   x[n] <- gsub("<c3><af><c2><bf><c2><bd>", "a", x[n])  # Ullev*a*l
  x[n] <- gsub("<c2><a1><c2><af>", "'", x[n])  # Xi'an
   x[n] <- gsub("<e2><80><93>", "-", x[n])
   x[n] <- gsub("<e2><80><99>", "'", x[n])
   x[n] <- gsub("<e2><80><9c>", "", x[n])  # double quotes
   x[n] <- gsub("<e2><80><9d>", "", x[n]) # double quotes
   x[n] <- gsub("<ef><bc><8c>", ",", x[n])


   x[n] <- gsub("<c2><81>", ",", x[n])

   x[n] <- gsub("<c3><a0>", "a", x[n])
   x[n] <- gsub("<c3><a1>", "a", x[n])
   x[n] <- gsub("<c3><a3>", "a", x[n]) 
   x[n] <- gsub("<c3><a4>", "a", x[n])
 
   x[n] <- gsub("<c3><a7>", "c", x[n])

   x[n] <- gsub("<c3><a8>", "e", x[n])
   x[n] <- gsub("<c3><a9>", "e", x[n])
   x[n] <- gsub("<c3><aa>", "e", x[n])

   x[n] <- gsub("<c3><ad>", "i", x[n])

   x[n] <- gsub("<c3><b3>", "o", x[n])
   x[n] <- gsub("<c3><b4>", "o", x[n])
   x[n] <- gsub("<c3><b6>", "o", x[n])

   x[n] <- gsub("<c3><ba>", "u", x[n])
   x[n] <- gsub("<c3><bc>", "u", x[n])
   x
}
