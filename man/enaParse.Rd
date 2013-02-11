\name{enaParse}
\alias{enaParse}
\title{ Parse XML from ENA browser }
\description{  Parse XML returned the  ENA taxonomy browser REST URLs  }
\usage{
enaParse(doc)
}
\arguments{
  \item{doc}{ an XML document with a "portal" attribute describing record type  }
}
\details{ Parses XML output returned from the ENA taxonomy browser REST url }
\value{  A data.frame }
\references{ The ENA browser REST services are described on the help page at \url{http://www.ebi.ac.uk/ena/about/browser} }
\author{ Chris Stubben }
%\note{ }


\seealso{ \code{\link{ena}}   }

\examples{
\dontrun{
x <- ena(777, "study") 
enaParse( x)
x <- ena(777, "sample") 
enaParse( x)

# HACK to parse EMBL records.  IF NOT using taxonomy portal, then add record type using portal attribute
doc <- ena("SRS281722,SRS269832")
attr(doc, "portal") <- "sample"
enaParse(doc)

# will also parse submissions 
doc <- ena("SRA048497,SRA036029,SRA047934")
attr(doc, "portal") <- "submission"
enaParse(doc)
}

}
\keyword{ methods }
