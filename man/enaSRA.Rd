\name{enaSRA}
\alias{enaSRA}
\title{ ENA sequence read archive  }
\description{ Search for SRA samples at the ENA using a taxonomy name or id}
\usage{
enaSRA(tax, limit = 5000)
}
\arguments{
  \item{tax}{ a taxonomy ID or name }
  \item{limit}{ total number of samples to return }
}
\details{ Searches the sra_sample data from the taxonomy portal at ENA. }
\value{ a data.frame listing SRA samples }
\references{ \url{http://www.ebi.ac.uk/ena/about/browser#taxonomy_portal_options} }
\author{ Chris Stubben }
\note{ URL strings at ENA require a taxonomy ID, so searching by name 
uses a \code{\link{ncbiTaxonomy}} ID lookup. }


\seealso{ \code{\link{sra}} for all microbial SRA samples and a description of columns.  Also 
see \code{\link{enaTaxonomy}}  to check the total number of SRAs before downloading}
\examples{
# chimps
pan<-enaSRA(9596)   # or pan<-enaSRA("Pan")
head(pan)
nrow(pan)
table2(pan$center)
bases(sum(pan$bases, na.rm=TRUE))
bases(sum(pan$reads, na.rm=TRUE), round=1)

}
\keyword{ methods }
