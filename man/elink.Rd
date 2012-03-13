\name{elink}
\alias{elink}
\title{ Entrez database links }
\description{ Find links between Entrez databases at NCBI  }
\usage{
elink(id, cmd = "neighbor_history", parse = TRUE, ...)
}
\arguments{
  \item{id}{ An EntrezHistory object or vector of Ids }
  \item{cmd}{ Command mode }
  \item{parse}{  Parse results into an EntrezHistory object (default) or 
      vector of linked Ids (if cmd="neighbor").  All other cmd options return XML }
  \item{\dots}{ Other key-value pairs passed to the elink url string }
}
\details{ See \code{\link{einfo} to find available links } }

\value{ Same as \code{\link{esearch}} }

\references{\url{ http://www.ncbi.nlm.nih.gov/books/NBK25499 } }
\author{ Chris Stubben}
%\note{ }
% \seealso{}
\examples{
\dontrun{
elink("15718680,157427902", dbfrom="protein", db="gene")
elink("15718680,157427902", dbfrom="protein", db="gene", cmd="neighbor")
# list linknames
einfo("genome", TRUE)[, 1:2]
x <- esearch("Nipah virus", "genome")
# genome_nuccore is default link. Links to reference AND genbank sequence the reference was derived from
esummary( elink(x, db="nuccore"))
# Link to other genomes
esummary( elink(x, db="nuccore", linkname="genome_nuccore_samespecies"))
}
}
\keyword{ methods }
