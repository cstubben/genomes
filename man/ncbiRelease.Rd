\name{ncbiRelease}
\Rdversion{1.1}
\alias{ncbiRelease}
\title{ NCBI revision history }
\description{ Returns the date a sequence was first seen at NCBI using the revision history display. }
\usage{
ncbiRelease(ids, db="nuccore", common=TRUE, random=20)
}

\arguments{
  \item{ids}{ A vector or comma-separated list of sequence accessions or GI numbers }
  \item{db }{ Entrez sequence database to search, default nuccore  }
  \item{common}{ If replaced sequences are found, search for the earliest date in the common revision history  }
  \item{random}{ The number of replaced sequences to search }
}
\details{ Searches the revision history display and parses the line 
  listing the date a sequence was \emph{first seen at NCBI}.
   In some cases, a sequence replaces earlier IDs and if the \code{common} option is TRUE,
   the earliest date of the replaced sequences is returned instead.  Also, since a sequence accession 
   may replace 500 or more ids, a random sample of the replaced sequences will be checked.  


 }
\value{
A data frame listing the accession, release date, and whether replaced sequences are found
}
%\references{ }
\author{ Chris Stubben }
%%\note{} 
%% \seealso{  \code{\link{ }}  }

\examples{
\dontrun{
#Yersinia pestis - 1 chromosome and 3 plasmids
ncbiRelease("AL590842,AL117189,AL109969,AL117211")
# or skip common revision history
ncbiRelease("AL590842", common=FALSE)
}
# Protein acc
ncbiRelease("CAA21395", db="protein")

}

\keyword{ methods }
