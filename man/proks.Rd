\name{proks}
\alias{proks}
\docType{data}
\title{ Prokaryotic genomes at NCBI }
\description{ Prokaryotic genome sequencing projects at NCBI. }
\usage{data(proks)}
\format{
  A genomes data frame with observations on the following 20 variables.
  \describe{
    \item{\code{acc}}{ BioProject id }
    \item{\code{name}}{ Organism name }
    \item{\code{status}}{ Complete, Assembly(=Scaffolds or contigs), SRA or No data}
    \item{\code{released}}{First public sequence release}
    \item{\code{taxid}}{ Taxonomy id }
    \item{\code{acc}}{ BioProject Accession number }
    \item{\code{group}}{Phylum}
    \item{\code{subgroup}}{Class level}
    \item{\code{size}}{Total length of DNA (Mb)}
    \item{\code{gc}}{Percent GC (guanine or cytosine)}
    \item{\code{refseq}}{Refseq chromosome sequence accessions}
    \item{\code{insdc}}{GenBank chromosome sequence accessions}
    \item{\code{prefseq}}{Refseq plasmid sequence accessions}
    \item{\code{pinsdc}}{GenBank plasmid sequence accessions}
    \item{\code{wgs}}{Four-letter Accession prefix followed by version}
    \item{\code{scaffolds}}{Number of scaffolds/contigs}
    \item{\code{genes}}{Number of genes}
    \item{\code{proteins}}{Number of proteins}
    \item{\code{modified}}{Last modification date}
    \item{\code{center }}{Sequencing center}
  }
}
% \details{ }
\source{
downloaded from \url{ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/prokaryotes.txt }
}
%\references{}
\examples{
data(proks)
proks
#single row 
t(proks[1,])
class(proks)
attributes(proks)[c("date","url")] 
summary(proks)
## check for missing release dates
table2(proks$status, !is.na(proks$released), dnn=list("Status", "Released Date?"))
#  table2(proks$status,!is.na(proks$wgs), dnn=list("Status", "Has WGS acc?"))
plot(proks)
plotby(proks, log='y', las=1, top=2)
hist(proks$size[proks$size<15], br=50, main="", col="blue", xlab="Size (Mb)")

## download recent table from NCBI
\dontrun{update(proks)} 


}
\keyword{datasets}
