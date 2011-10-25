\name{ncbiInfo}
\alias{ncbiInfo}
\title{ NCBI Entrez databases and fields
}
\description{ Return a list all Entrez databases or the indexing fields and available links for a single database
}
\usage{
ncbiInfo(db, list = "field")
}
\arguments{
  \item{db}{ a valid Entrez database , if missing then all databases are listed}
  \item{list}{ list database fields or links }
}
\details{ Runs EInfo and parses XML results }
\value{ A data.frame listing databases, fields, or links }
\references{
 \url{ http://www.ncbi.nlm.nih.gov/books/NBK25499 }
}
\author{ Chris Stubben }
% \note{ }


% \seealso{ }
\examples{
ncbiInfo()
ncbiInfo("bioproject")
ncbiInfo("bioproject", "link")


}
\keyword{ methods }
