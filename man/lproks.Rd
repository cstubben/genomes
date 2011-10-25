\name{lproks}
\alias{lproks}
\docType{data}
\title{ Microbial genome projects at NCBI}
\description{
 Microbial genomes from Entrez genome project at NCBI.
}
\usage{data(lproks)}
\format{
  A genomes data frame with observations on the following 31 variables.
  \describe{
    \item{\code{pid}}{ genome project id}
    \item{\code{name}}{taxonomy name}
   \item{\code{status}}{ sequencing status, Complete, Assemby, or
      In Progress genomes}
    \item{\code{released}}{released date, complete and WGS genomes
        only}
    \item{\code{refseq_pid}}{ RefSeq project id}    
    \item{\code{taxid}}{taxonomy id}
    \item{\code{kingdom}}{kingdom}
    \item{\code{group}}{phylum or class}
    \item{\code{size}}{genome size (Mbp)}
    \item{\code{GC}}{percent GC content}
    \item{\code{chromosomes}}{number of chromosomes, complete genomes only}
    \item{\code{plasmids}}{number of plasmids, complete genomes only}
    \item{\code{modified}}{modified date, complete genomes only}
    \item{\code{genbank}}{comma-separated list of GenBank accession numbers}
    \item{\code{refseq}}{comma-separated list of RefSeq accession numbers}
    \item{\code{publication}}{comma-separated list of PubMed ids, complete genomes only}
    \item{\code{center}}{pipe-separated list of sequencing centers}
    \item{\code{contigs}}{number of genome contigs.  For complete
        genomes, contigs are the sum of chromosomes and plasmids }
    \item{\code{cds}}{ number of coding sequences, WGS only }
    \item{\code{url}}{sequencing center url, WGS and In Progress
        genomes only }
    \item{\code{gram}}{gram stain }
    \item{\code{shape}}{ shape }
    \item{\code{arrange}}{ arrangement}
    \item{\code{endospore}}{ endospores }
    \item{\code{motility}}{ motility}
    \item{\code{salinity}}{ salinity}
    \item{\code{oxygen}}{ oxygen requirement}
    \item{\code{habitat}}{ habitat}
    \item{\code{temp}}{ temperature preference }
    \item{\code{range}}{ temperature range}
    \item{\code{pathogen}}{ pathogenic in host }
    \item{\code{disease}}{ disease}

  }
}
\details{ This table is constructed using all three
tabs at \url{http://www.ncbi.nlm.nih.gov/genomes/lproks.cgi}.
Complete genomes and In Progress tabs are combined and then joined to
the Organism Info tab.  A few manual updates were also added:
 725 missing released dates from GenBank assemblies were added, 178 complete 
 genomes with assembly released dates were corrected (see \code{\link{complete}}, 
 and genome size outliers were removed (these changes will not be kept in new updates below)


The \code{update(genomes)} function downloads a recent copy of the
table from NCBI.  The number of new project IDs are reported as well
as the number of project IDs removed (which are typically Assembly
genomes that are now available as a Complete sequence).    }

\source{
 downloaded from \url{http://www.ncbi.nlm.nih.gov/genomes/lproks.cgi}
}
% \references{}
\examples{
data(lproks)
lproks
#single row (long format)
t(lproks[1,])
class(lproks)
## download stats
attributes(lproks)[c("stats", "date","url")] 
summary(lproks)
## check for missing release dates
table2(!is.na(lproks$released), lproks$status, dnn=list("Released Date?", "Status"))
plot(lproks)
plotby(lproks, log='y', las=1)
## download recent table from NCBI
\dontrun{update(lproks)} 
## Yersinia genomes
yp <- subset(lproks, name \%like\% 'Yersinia*')
yp
summary(yp)
plotby(yp, labels=TRUE, cex=.7, lbty='n')
}
\keyword{datasets}
