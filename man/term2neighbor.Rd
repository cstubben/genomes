\name{term2neighbor}
\Rdversion{1.1}
\alias{term2neighbor}
\title{  Retrieve genome neighbors from NCBI  }
\description{  Search Entrez Genome at NCBI and retrieve links
  (other genomes for species) to the nucleotide database using Entrez programming
  utilities (eUtils) }
\usage{
term2neighbor(term, derived = FALSE, sortdate = FALSE, fulltable = FALSE)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms }
  \item{derived}{  Include GenBank sequences that the Reference
      sequences were derived from (default is only the neighbors in
      genome_nuccore_samespecies)  }
  \item{sortdate}{ Sort the results by released date (default is by name) }
  \item{fulltable}{ Return all  12 summary fields   }
}
\details{ The functions searches the Genome database using the ESearch utility,
  finds links to Other Genomes for Species using ELink,  returns document summary
  pages using ESummary, and then parses the XML fields using the \code{XML} package  }

\value{ A genomes data frame with 5 columns (acc, name (defline), released date,
  taxid, and size).  If fulltable is TRUE, then all fields are returned } 

\references{ A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}. }
\author{ Chris Stubben }
\note{This function will most likely be useful for viral sequences,
  which typically have only one reference sequence per species, and other
  strains are linked as Genome Neighbors.  
}

\seealso{  \code{\link{virus}}  }
\examples{
data(virus)
## Nipah virus list 7 neighbors
subset(virus, name \%like\% 'Nipah*')
# term2neighbor('Nipah virus[orgn]')
#  if plotting, also  include the genbank sequence that reference was derived from
x <- term2neighbor('Nipah virus[ORGN]', derived = TRUE)
x
plot(x, ylab = 'Nipah virus sequences')
}

\keyword{ methods }
