\name{ncbiGenome}
\Rdversion{1.1}
\alias{ncbiGenome}
\title{  NCBI Genome Database  }
\description{  Search Entrez Genome at NCBI and retrieve genomes or linked neighbors
  in the Nucleotide database }
\usage{
ncbiGenome(term, neighbors=FALSE, derived = TRUE, fulltable = FALSE)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms }
  \item{neighbors}{ Search for genome neighbors in Entrez Nucleotide}
  \item{derived}{  If searching neighbors, also include the GenBank assembly that the Reference
      sequence was derived from  }
  \item{fulltable}{ Return all summary fields   }
}
\details{ Returns summaries in Entrez Genome or if neighbor is TRUE, then links to Entrez Nucleotide 
using genome_nuccore_samespecies (Other genomes for species) and genome_nuccore (Assembly).  See \code{ncbiInfo("genome", "links")} }
\value{ A genomes data frame with 5 columns (acc, name, released date,
  taxid, and size).  If fulltable is TRUE, then all summary fields are returned } 

\references{ A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}. }
\author{ Chris Stubben }
\note{ Viral sequences typically have only one reference sequence per species in Entrez genome, and other
  strains are linked as Genome Neighbors.  
}

%\seealso{    }

\examples{
 ncbiGenome('Nipah virus[orgn]')
 x<-ncbiGenome('Nipah virus[ORGN]', neighbors = TRUE)
 x[1:2,]
 summary(x)
}

\keyword{ methods }
