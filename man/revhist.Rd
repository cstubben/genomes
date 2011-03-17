\name{revhist}
\Rdversion{1.1}
\alias{revhist}
\docType{data}
\title{
Complete microbial genome release dates in Revision History
}
\description{
Lists the date a sequence was  \emph{first seen} using genbank accessions
from complete microbial genomes in the Seqeunce Revision History database at NCBI.
}
\usage{data(revhist)}
\format{
  A data frame with 1485 observations on the following 3 variables.
  \describe{
    \item{\code{id}}{genbank accession number}
    \item{\code{released}}{ date sequence was first seen}
    \item{\code{common}}{ was common revision history link used?}
  }
}
\details{
This table was created by taking the \emph{first} genbank accession in the
\code{\link{lproks}} table  and using
\code{\link{acc2date}} to return the date the sequence was first seen
at NCBI.  In some cases, the genome sequence may not be the first
genbank ID in the list (eg, a plasmid sequence may be first) and no
attempt was made to correct these rows.  


}
\source{
 Sequence Revision History database at NCBI \url{http://www.ncbi.nlm.nih.gov/sviewer/girevhist.cgi} 
}
%\references{ }
\examples{
data(revhist)

# sorted by release date in lproks
head(revhist)
data(lproks)
x<-subset(lproks, year(released)==1995, c(name,genbank))
x
# genome sequence BA000022 for Synechocystis is 4th id in list
acc2date(x$genbank)

}
\keyword{datasets}
