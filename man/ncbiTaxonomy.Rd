\name{ncbiTaxonomy}
\alias{ncbiTaxonomy}
\title{  NCBI taxonomy database  }
\description{ Search the Entrez taxonomy database at NCBI 
 and return summaries using Esummary or Efetch
}
\usage{
ncbiTaxonomy(term, results = "summary", full = FALSE)
}
\arguments{
  \item{term}{ either a valid Entrez search term or a vector of taxonomy IDs (passed to Epost) or names (joined using OR)  }
  \item{results}{ return a summary using Esummary (default) or lineage using Efetch}
  \item{full}{ return all 14 Esummary fields, default is 9 }
}
\details{ The function uses either Epost (for numeric taxonomy IDs)
or Esearch to query the taxonomy database and then passes the results to
either Esummary or Efetch. Esummary returns the id, name, rank, division
and number of linked records in various Entrez databses.  Efetch returns the id, name,
rank, lineage and date.
 }
\value{ a data.frame }
\references{ NCBI taxonomy database \url{http://www.ncbi.nlm.nih.gov/sites/entrez?db=taxonomy}  }
\author{ Chris Stubben }
%\note{ }

\seealso{ \code{\link{ncbiInfo}} for a list of fields in the taxonomy database }
\examples{
ncbiTaxonomy("cellular organisms[Next Level]")  
# new Hantavirus species in 2011
ncbiTaxonomy("Hantavirus[subtree] AND 2011[date] AND species[rank]") 
# difference between summary and lineage (Efetch) results
#ncbiTaxonomy (1000587)
#ncbiTaxonomy (1000587, "lineage")


}
\keyword{ methods }
