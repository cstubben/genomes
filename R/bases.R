bases <-function(x, round=0)
{
ifelse( x< 10^3, paste(x, "bp"),
 ifelse(x< 10^6, paste(round( x/ 10^3, round) , "kb"),
   ifelse(x< 10^9, paste(round( x/ 10^6, round) , "Mb"), 
     ifelse(x< 10^12, paste(round( x/ 10^9, round) , "Gb"), 
       paste(round( x/ 10^12, round) , "Tb") ))))
}
