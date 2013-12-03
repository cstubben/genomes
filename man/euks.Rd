\name{euks}
\alias{euks}
\docType{data}
\title{ Eukaryotic genomes at NCBI }
\description{ Eukaryotic genome sequencing projects at NCBI
}
\usage{data(euks)}
\format{
  A genomes data frame with observations on the following 21 variables.
  \describe{
    \item{\code{acc}}{ BioProject id }
    \item{\code{name}}{ Organism name }
    \item{\code{status}}{ Chromosome, Scaffolds or contigs, SRA or No data}
    \item{\code{released}}{First public sequence release}
    \item{\code{taxid}}{ Taxonomy id }
   \item{\code{acc}}{ BioProject Accession number }
    \item{\code{group}}{Phylum}
    \item{\code{subgroup}}{Class level}
    \item{\code{size}}{Total length of DNA (Mb)}
    \item{\code{gc}}{Percent GC (guanine or cytosine)}
    \item{\code{assembly}}{Name of the genome assembly (from NCBI Assembly database)}
    \item{\code{chromosomes}}{Number of chromosomes }
    \item{\code{organelles}}{Number of organelles}
    \item{\code{plasmids}}{Number of plasmids}
    \item{\code{wgs}}{Four-letter Accession prefix followed by version}
    \item{\code{scaffolds}}{Number of scaffolds}
    \item{\code{genes}}{Number of genes}
    \item{\code{proteins}}{Number of proteins}
    \item{\code{modified}}{Last modification date}
    \item{\code{center }}{Sequencing center}
    \item{\code{biosample }}{BioSample Accession number} 
  }
}
\details{Excludes projects that represent only organelles}
\source{
downloaded from \url{ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/eukaryotes.txt }
}
%\references{}
\examples{
data(euks)
euks
t(euks[1,])
plot(euks)
summary(euks)
table2(euks$subgroup)
# table2(euks$subgroup, euks$status)

}
\keyword{datasets}
