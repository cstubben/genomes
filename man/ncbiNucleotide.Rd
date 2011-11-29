\name{ncbiNucleotide}
\alias{ncbiNucleotide}
\title{ NCBI Nucleotide database }
\description{  Search Entrez Nucleotide at NCBI and retrieve summary tables }
\usage{
ncbiNucleotide(term, fulltable = FALSE)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms or a vector of accessions numbers }
  \item{fulltable}{ Return all summary fields }
}
\details{ Returns a summary from Entrez Nucleotide.  }
\value{ A genomes data frame with acc, name, released, taxid, size and gi.  }
\references{ A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}.
}
\author{ Chris Stubben }
%\note{  }

\seealso{ \code{\link{ncbiGenome}} }
\examples{
ncbiNucleotide("AL117189,AL109969,AL117211")
# Exclude Patents and Refseq which are usually duplicates
marb <- ncbiNucleotide( "Marburgvirus[ORGN] NOT gbdiv_pat[PROP] NOT srcdb_refseq[PROP]")
marb
# two peaks in size distribution (partial and complete sequences)
hist(marb$size, col="blue", br=30, main="Marburg virus sequences", xlab="Length (bp)")

# Compare to linked NCBI Genomes (20 genomes)
marbg <-ncbiGenome("Marburgvirus[ORGN]")
# Remove "nucleoprotein (NP)... genes" from 3 long deflines for display
marb$name <- gsub("(.*)( nucleoprotein.*)(, complete.*)", "\\1\\3",  marb$name)
# 13 genomes out of 33 missing links to Entrez Genome 
data.frame(subset(marb, size > 16000 & !acc \%in\% marbg$acc))

}
\keyword{ methods}
