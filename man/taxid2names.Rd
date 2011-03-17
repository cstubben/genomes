\name{taxid2names}
\Rdversion{1.1}
\alias{taxid2names}

\title{
Retreive taxonomy names from NCBI
}
\description{
Search the Entrez taxonomy database at NCBI  and
return names and lineages for valid taxonomy ids
}
\usage{
taxid2names(ids)
}
\arguments{
  \item{ids}{ a vector of NCBI taxonomy ids }
}
\details{
The function searches the Taxonomy database using the EFetch utility
and returns an XML summary report, and then parses the name and lineage fields
}
\value{  A dataframe listing taxonomy id, name and lineage}
% \references{ }
\author{ Chris Stubben }
%\note{ }
%\seealso{ }
\examples{
taxid2names(2)
x <- taxid2names(c(280855, 11595, 273349))
# remove common parents
x$lineage<- gsub("Viruses; ssRNA viruses; ssRNA negative-strand viruses; Bunyaviridae; ", "", x$lineage)
x
}

\keyword{ methods }
