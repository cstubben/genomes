\name{acc2date}
\Rdversion{1.1}
\alias{acc2date}
\title{
Retrieve release dates using NCBI's revision history display settings
}
\description{
Returns the date a sequence was first seen at NCBI using the revision
history display.
}
\usage{
acc2date(ids, db="nuccore")
}

\arguments{
  \item{ids}{  a vector or comma-separated list of  sequence accessions or GI numbers }
  \item{db }{ Entrez database to search, default nuccore  }
}
\details{ Searches the  revision history display  and parses the line listing the date a sequence was \emph{first seen at
    NCBI}. In many cases, a sequence replaces earlier IDs and the
  function will print a warning to check the common history using \code{\link{acc2dateCommon}}.
 }
\value{
A data frame listing the sequence identifier and release date
}
%\references{ }
\author{
Chris Stubben
}
%%\note{} 
\seealso{  \code{\link{acc2dateCommon}}  }

\examples{
data(lproks)
yp<-subset(lproks, name \%like\% 'Yersinia*CO92')
yp$genbank
# 1 chromosome and 3 plasmids
acc2date(yp$genbank)

# protein acc
acc2date("CAA21395", db="protein")

}

\keyword{ methods }
