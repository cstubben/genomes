\name{virus}
\Rdversion{1.1}
\alias{virus}
\docType{data}
\title{
  Virus genomes at NCBI
}
\description{
 Viral reference genome sequencing projects at NCBI
}
\usage{data(virus)}
\format{
  A genomes data frame with the following 13 variables.
  \describe{
    \item{\code{acc}}{ BioProject Accession number }
    \item{\code{name}}{ Organism name }
    \item{\code{status}}{ Highest level of assembly; Complete, SRA or No data}
    \item{\code{released}}{First public sequence release}
    \item{\code{group}}{Phylum}
    \item{\code{subgroup}}{Class level}
    \item{\code{size}}{Total length of DNA (Mb)}
    \item{\code{gc}}{Percent GC (guanine or cytosine)}
    \item{\code{host}}{ Natural host of a virus}
    \item{\code{segments}}{ Number of segments}
    \item{\code{genes}}{Number of genes}
    \item{\code{proteins}}{Number of proteins}
    \item{\code{modified}}{Sequence modification date}
  }
}
\details{
Includes only data represented in the RefSeq dataset.

}
\source{
downloaded from \url{ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/viruses.txt}
}
%\references{}
\examples{
data(virus)
plot(virus)
summary(virus)
table(virus$segments)
table2(virus$host)
## most common phages
table2(species(grep("phage", virus$name, value=TRUE)))
# TABLE only includes RefSeq genomes - see ncbiGenome for links
subset(virus, name=="Nipah virus")
ncbiGenome('Nipah virus[ORGN]')
}
\keyword{datasets}
