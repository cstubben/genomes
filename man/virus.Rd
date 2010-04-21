\name{virus}
\Rdversion{1.1}
\alias{virus}
\docType{data}
\title{
  Virus genomes at NCBI
}
\description{
 Viral reference genome sequencing projects at NCBI.

}
\usage{data(virus)}
\format{
  A genomes data frame with the following 8 variables.
  \describe{
    \item{\code{name}}{virus name}
    \item{\code{released}}{ release date}
    \item{\code{neighbors}}{number of Genome Neighbors}
    \item{\code{segments}}{number of segments}
    \item{\code{refseq}}{RefSeq accession number }
    \item{\code{isolate}}{ isolate name}
    \item{\code{size}}{ genome size (nt)}
    \item{\code{proteins}}{number of proteins}
  }
}
\details{
Please refer to the Viral genomes page at NCBI
\url{http://www.ncbi.nlm.nih.gov/genomes/GenomesHome.cgi?taxid=10239&hopt=aboutsite}
for details on Reference genomes.  One Reference genome is selected
per viral species and other strains are linked as Genome Neighbors
(other complete sequences for the species).  See the
\code{\link{term2neighbor}} function to get a list of Genome
neighbors.

Summing the number of segments in this table should return the total
number of reference sequences; however, summing the number of genome
neighbors will not return the number of linked GenBank sequences since
many counts are duplicated or missing (eg, Dengue virus neighbors are
listed 4 times, Influenza A and B neighbors are missing.

}
\source{
downloaded from \url{http://www.ncbi.nlm.nih.gov/genomes/GenomesGroup.cgi?taxid=10239&opt=Virus&sort=genome}
}
%\references{}
\examples{
data(virus)
plot(virus)
summary(virus)
sum(virus$segments)
# some neighbors repeat (others are missing)
subset(virus, name \%like\% 'Dengue*')
subset(virus, name \%like\% 'Monkey*')
# list the neighbors
term2neighbor("Monkeypox virus[orgn]")

## most common phages
table2(species(grep("phage", virus$name, value=TRUE)))


}
\keyword{datasets}
