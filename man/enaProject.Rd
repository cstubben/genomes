\name{enaProject}
\alias{enaProject}
\title{ ENA projects }
\description{Search for projects at ENA using a taxonomy name or id}
\usage{
enaProject(tax, limit = 1000, refseq = TRUE)
}
\arguments{
  \item{tax}{a taxonomy ID or name }
  \item{limit}{total number of projects to return }
  \item{refseq}{ include RefSeq projects }
}
\details{Searches the project data from the taxonomy portal at ENA. }
\value{ a data.frame listing projects and submission details }
\references{ \url{ http://www.ebi.ac.uk/ena/about/browser#taxonomy_portal_options } }
\author{ Chris Stubben }
\note{ URL strings at ENA require a taxonomy ID, so searching by name 
uses a \code{\link{ncbiTaxonomy}} ID lookup at NCBI. 
}


\seealso{\code{\link{enaSRA} to search for SRA samples.}
}
\examples{
pan<-enaProject(9596)
pan


}
\keyword{ methods }
