\name{ncbiGenome}
\Rdversion{1.1}
\alias{ncbiGenome}
\title{  NCBI Genome Database  }
\description{  Search Entrez Genome at NCBI and retrieve genome sequences
  in the Nucleotide database }
\usage{
ncbiGenome(term, refseq=FALSE, fulltable = FALSE)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms }
  \item{refseq}{ Return only RefSeq genomes, default is GenBank submissions  }
  \item{fulltable}{ Return all summary fields   }
}
\details{  Searches Entrez Genome and finds linked sequences in Entrez Nucleotide 
using genome_nuccore (Assembly) and then finds related sequences using 
nuccore_nuccore_samespecies_rsgb (Other INSDC Genome Sequences). }
\value{ A data frame with 6 columns (acc, name, released date,
  taxid, size, and gi).  If fulltable is TRUE, then all summary fields are returned } 

\references{ A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}. }
\author{ Chris Stubben }
\note{ The script was updated after the new Entrez Genome release on November 9, 2011. See 
url{http://www.ncbi.nlm.nih.gov/About/news/17Nov2011.html} for details.

}

%\seealso{    }

\examples{
 ncbiGenome('Nipah virus[orgn]')
 ncbiGenome('Nipah virus[orgn]', refseq=TRUE)
}

\keyword{ methods }
