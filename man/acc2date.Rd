\name{acc2date}
\Rdversion{1.1}
\alias{acc2date}
\title{
Retrieve release dates from NCBI's revision history
}
\description{
Returns the date a sequence was first seen at NCBI using the revision history.
}
\usage{
acc2date(ids, common=TRUE)
}

\arguments{
  \item{ids}{  a vector or comma-separated list of  sequence accessions or GI numbers }
  \item{common }{ if present, use the common revision history link }
}
\details{ Searches the sequence revision history at NCBI
  \url{http://www.ncbi.nlm.nih.gov/sviewer/girevhist.cgi} and parses
  the line listing the date a sequence was \emph{first seen at
    NCBI}. In many cases, a sequence replaces earlier IDs and will
  therefore include a common revision history link.  If this option is set, each
  common history link is searched and the earliest release date from the
  collection of  sequence ids is returned instead.
 }
\value{
A data frame listing the sequence identifier, release date, and if
common revision history was used.
}
%\references{ }
\author{
Chris Stubben
}
%%\note{} 
\seealso{  \code{\link{revhist}}  }

\examples{
data(lproks)
yp<-subset(lproks, name \%like\% 'Yersinia*CO92')
yp$genbank
# 1 chromosome and 3 plasmids
acc2date(yp$genbank)

acc2date(yp$genbank, common=FALSE)

}

\keyword{ methods }
