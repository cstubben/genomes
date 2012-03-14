\name{ncbiTaxonomy}
\alias{ncbiTaxonomy}
\title{  NCBI taxonomy database  }
\description{ Search the Entrez taxonomy database at NCBI
}
\usage{
ncbiTaxonomy(term, summary=TRUE)
}
\arguments{
  \item{term}{ either a valid Entrez search term or a vector of taxonomy Ids or names  }
  \item{summary }{ return results using Esummary (default) or Efetch}
}
\details{
This function uses either Esummary or Efetch to return taxonomy data from NCBI.  The Efetch XML 
include parent ids and lineage tags not found in Esummary XML.  The term may be also be a vector 
of taxonomy Ids (joined using a comma) or taxonomy names (joined using "OR").  
 }
\value{ a data.frame }
\references{ NCBI taxonomy database \url{http://www.ncbi.nlm.nih.gov/sites/entrez?db=taxonomy}  }
\author{ Chris Stubben }
%\note{ }

\seealso{ \code{\link{einfo}} for a list of fields in the taxonomy database.  }

\examples{

ncbiTaxonomy(c("Bacillus anthracis", "Yersinia pestis"))
ncbiTaxonomy("cellular organisms[Next Level]")  
# new Hantavirus species added in 2012
ncbiTaxonomy("Hantavirus[subtree] AND 2012[date] AND species[rank]") 
# efetch results
ncbiTaxonomy (1145238, FALSE)
# can also use Lineage field
ncbiTaxonomy("Necocli virus[Lineage]")


}
\keyword{ methods }
