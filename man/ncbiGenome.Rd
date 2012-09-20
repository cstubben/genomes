\name{ncbiGenome}
\Rdversion{1.1}
\alias{ncbiGenome}
\title{ NCBI Genome links to the Nucleotide database}
\description{  Search Entrez Genome at NCBI and retrieves linked genomes
  in the Nucleotide database }
\usage{
ncbiGenome(term, refseq=FALSE)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms }
  \item{refseq}{ Include RefSeq genomes, default is GenBank submissions  }
}
\details{  Searches Entrez Genome and finds linked sequences in Entrez Nucleotide 
using genome_nuccore (Assembly) and then finds related sequences using 
nuccore_nuccore_samespecies_rsgb (Other INSDC Genome Sequences).  The genome_nuccore link
includes the Reference and Genbank acc that Reference was derived from (and refseq option is used to exclude 
duplicate RefSeq from results).  }
\value{ A genomes data frame with acc, name, created,
  taxid, size, gi and other fields.  } 

\references{ A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}. }
\author{ Chris Stubben }
% \note{ }

%\seealso{    }

\examples{

\dontrun{
ncbiGenome('Nipah virus[orgn]')
ncbiGenome('Nipah virus[orgn]', refseq=TRUE)
}

}

\keyword{ methods }
