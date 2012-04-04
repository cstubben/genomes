\name{proks}
\alias{proks}
\docType{data}
\title{ Prokaryotic genomes at NCBI }
\description{ Prokaryotic genome sequencing projects at NCBI. }
\usage{data(proks)}
\format{
  A genomes data frame with observations on the following 17 variables.
  \describe{
    \item{\code{acc}}{ BioProject Accession number }
    \item{\code{name}}{ Organism name }
    \item{\code{status}}{ Highest level of assembly; Complete, Assembly, SRA or No data}
    \item{\code{released}}{First public sequence release}
    \item{\code{group}}{Phylum}
    \item{\code{subgroup}}{Class level}
    \item{\code{size}}{Total length of DNA (Mb)}
    \item{\code{gc}}{Percent GC (guanine or cytosine)}
    \item{\code{refseq}}{Refseq chromosome sequence accessions}
    \item{\code{insdc}}{GenBank chromosome sequence accessions}
    \item{\code{prefseq}}{Refseq plasmid sequence accessions}
    \item{\code{pinsdc}}{GenBank plasmid sequence accessions}
    \item{\code{wgs}}{Four-letter Accession prefix followed by version}
    \item{\code{scaffolds}}{Number of scaffolds}
    \item{\code{genes}}{Number of genes}
    \item{\code{proteins}}{Number of proteins}
    \item{\code{modified}}{Sequence modification date}
  }
}
\details{Excludes projects that represent only plasmids}
\source{
downloaded from \url{ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/prokaryotes.txt }
}
%\references{}
\examples{
data(proks)
proks
#single row (long format)
t(proks[1,])
class(proks)
attributes(proks)[c("date","url")] 
summary(proks)
## check for missing release dates
table2(!is.na(proks$released), proks$status, dnn=list("Released Date?", "Status"))
plot(proks)
plotby(proks, log='y', las=1, top=2)
## download recent table from NCBI
\dontrun{update(proks)} 


}
\keyword{datasets}
