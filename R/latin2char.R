latin2char<-function(x)
{
   n <- is.na(iconv(x, "latin1", "ASCII"))
   x[n] <- iconv(x[n], "latin1", "ASCII", sub="byte")

   ## missing hex codes in some lproks centers
   x[n] <- gsub("Troms<ef><bf><bd>", "Tromso", x[n])
   x[n] <- gsub("Unit<ef><bf><bd>",  "Unite", x[n])
   x[n] <- gsub("<ef><bf><bd>", "?", x[n])  ## any others

   ## google search latin1 table for hex codes
   x[n] <- gsub("<96>", "-", x[n])   # dash in lenvs name
   x[n] <- gsub("<b0>", " ", x[n])   # degree symbol in lproks range
   
   ## maybe add upper case if needed - 
   x[n] <- gsub("<e0>", "a", x[n])
   x[n] <- gsub("<e1>", "a", x[n])
   x[n] <- gsub("<e2>", "a", x[n])
   x[n] <- gsub("<e3>", "a", x[n])   # lproks center
   x[n] <- gsub("<e4>", "a", x[n])   # lproks center
   x[n] <- gsub("<e5>", "a", x[n])   # lproks center
   x[n] <- gsub("<e6>", "ae", x[n])

   x[n] <- gsub("<e7>", "c", x[n])   # lproks center

   x[n] <- gsub("<e8>", "e", x[n])   # lproks center
   x[n] <- gsub("<e9>", "e", x[n])   # lproks center
   x[n] <- gsub("<e9>", "e", x[n]) 
   x[n] <- gsub("<ea>", "e", x[n]) 
   x[n] <- gsub("<eb>", "e", x[n]) 

   x[n] <- gsub("<ec>", "i", x[n]) 
   x[n] <- gsub("<ed>", "i", x[n])   # lproks center
   x[n] <- gsub("<ee>", "i", x[n])  
   x[n] <- gsub("<ef>", "i", x[n])  

   x[n] <- gsub("<f1>", "n", x[n])

   x[n] <- gsub("<f2>", "o", x[n])
   x[n] <- gsub("<f3>", "o", x[n])   # lproks center
   x[n] <- gsub("<f4>", "o", x[n])
   x[n] <- gsub("<f5>", "o", x[n])
   x[n] <- gsub("<f6>", "o", x[n])   # lproks center
   x[n] <- gsub("<f8>", "o", x[n])

   x[n] <- gsub("<f9>", "u", x[n])
   x[n] <- gsub("<fa>", "u", x[n])   # lproks center
   x[n] <- gsub("<fb>", "u", x[n])
   x[n] <- gsub("<fc>", "u", x[n])   # lproks center


   x
}
