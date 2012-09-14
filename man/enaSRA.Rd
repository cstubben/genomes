\name{enaSRA}
\alias{enaSRA}
\title{ ENA sequence read archive  }
\description{ Search for SRA samples at the ENA using a taxonomy name or id}
\usage{
enaSRA(tax, limit = 500)
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


\seealso{   See \code{\link{enaTaxonomy}}  to check the total number of SRAs before downloading}
\examples{
# chimps
pan<-enaSRA(9596)   # or pan<-enaSRA("Pan")
head(pan)
nrow(pan)
table2(pan$center)
bases(sum(pan$bases, na.rm=TRUE))
bases(sum(pan$reads, na.rm=TRUE), round=1)

# some taxonomy ID links are not working -- should be fixed in October 2012 ENA update
# in the meantime add limit to avoid downloading default (500) records.
# For example, 90 SRA samples from Cyanobacteria on Sep 14, 2012
enaTaxonomy(1117)
cyano <- enaSRA(1117, limit=90)
table2(genus(cyano$name))

}
\keyword{ methods }
