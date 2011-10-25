\name{ncbiSubmit}
\alias{ncbiSubmit}
\title{ NCBI submission dates }
\description{ Returns the date a sequence was submitted to NCBI using the 
 Direct Submission line in the GenBank file
}
\usage{
ncbiSubmit(term, db = "nuccore", retmax = 1000)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms or a vector of accessions numbers }
  \item{db}{Entrez sequence database to search, default nuccore  }
  \item{retmax}{ Number of records to return }
}
\details{
Searches an Entrez sequence database, downloads GenBank files and parses the 
JOURNAL line containing a submitted date, for example, \code{JOURNAL   Submitted (03-SEP-1999) ...}. 

}
\value{ a data.frame with accession, definition, and submitted date }
%\references{ }
\author{ Chris Stubben }
\note{ If more than two submitted dates are found, then the earliest date is returned }

\seealso{ \code{\link{ncbiRelease}}) 
}
\examples{
#Yersinia pestis in Genome database
ncbiSubmit("Yersinia pestis CO92[ORGN]", db="genome")

# Virus in nucleotide database
ebola<- ncbiSubmit("Ebolavirus[ORGN] NOT gbdiv_pat[PROP] NOT refseq[FILTER]")
head(ebola)
# a few early submissions may be missing
subset(ebola, is.na(submitted))
table(year(ebola$submit))

}
\keyword{ methods }
