\name{acc2dateCommon}
\Rdversion{1.1}
\alias{acc2dateCommon}
\title{
Retrieve earliest release date from replaced sequences in NCBI's revision history
}
\description{
Returns the earliest date that a set of replaced sequences was first seen at NCBI using the revision
history display.  Previously known as the common revision history.
}
\usage{
acc2dateCommon(id, random = 25)
}
\arguments{
  \item{id}{ nucleotide sequence accession }
  \item{random}{ the number of replaced sequences to search }
}
\details{ 
  On the defunct sequence revision history pages (girevhist.cgi),
  there was a "Common revision history'' link to search all replaced
  seqeunces.  In the new Entrez display, there are separate lines
  listing replaced sequences and each must be searched  separately. In some cases, a sequence accession may replace 500 or more
ids and therefore this function searches a random sample of
  the replaced sequences.  
}
\value{ the earliest release date }
%\references{}
\author{ Chris Stubben }
\note{
Currently only the nucleotide database can be searched.
}

\seealso{ \code{\link{acc2date}} }
\examples{
# Yersinia pestis CO92
acc2dateCommon("AL590842")
}
\keyword{ methods }
