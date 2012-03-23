\name{einfo}
\alias{einfo}
\title{ Entrez database information
}
\description{ List all Entrez databases at NCBI or the indexing fields and available links for a specific database
}
\usage{
einfo(db, links=FALSE)
}
\arguments{
  \item{db}{ a valid Entrez database, if missing then all databases are listed}
  \item{links}{ list database links, default is fields }
}
\details{ Runs Einfo and parses XML results }
\value{ A data.frame listing databases, fields, or links }
\references{
 \url{http://www.ncbi.nlm.nih.gov/books/NBK25499 }
}
\author{ Chris Stubben }
% \note{ }
% \seealso{ }
\examples{
einfo()
einfo("bioproject")
einfo("bioproject", TRUE)


}
\keyword{ methods }
