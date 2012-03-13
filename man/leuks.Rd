\name{leuks}
\alias{leuks}
\docType{data}
\title{ Eukaryotic genome projects at NCBI}
\description{
  Eukaryotic genome sequencing projects  at NCBI
}
\usage{data(leuks)}
\format{
  A genomes data frame with observations on the following 20 variables.
  \describe{
    \item{\code{pid}}{ genome project id }
    \item{\code{name}}{ taxonomy name}
    \item{\code{status}}{ sequencing status}
    \item{\code{released}}{ released date}
    \item{\code{group}}{ taxonomy group (animals, fungi, protists, or plants) }
    \item{\code{subgroup}}{ taxonomy subgroup}
    \item{\code{taxid}}{taxonomy id }
    \item{\code{size}}{genome size (Mbp) }
    \item{\code{chromosomes}}{number of chromosomes }
    \item{\code{method}}{ sequencing method}
    \item{\code{depth}}{ depth or coverage }
    \item{\code{center}}{ pipe-separated list of sequencing centers}
    \item{\code{genbank}}{ has GenBank sequences}
    \item{\code{pubmed}}{ has PubMed}
    \item{\code{refseq}}{ has RefSeq sequences}
    \item{\code{gene}}{ has Gene link}
    \item{\code{traces}}{ has Traces}
    \item{\code{blast}}{ has Blast page }
    \item{\code{mapview}}{ has MapView}
    \item{\code{ftp}}{ comma-separated list of ftps}
  }
}
\details{ 
This table is no longer supported by NCBI.  See \url{http://www.ncbi.nlm.nih.gov/About/news/17Nov2011.html} for details.
 }
\source{
  downloaded from Entrez genome project at \url{http://www.ncbi.nlm.nih.gov/genomes/leuks.cgi}
}
% \references{ }
\examples{
data(leuks)
leuks
# single row, long format
t(leuks[1,])
plot(leuks)
summary(leuks)
dotchart(sort(table(leuks$subgroup)), pch=16, xlab="Genome projects")

}
\keyword{datasets}
