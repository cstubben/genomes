\name{ena}
\alias{ena}
\title{ ENA browser REST URL }
\description{ Retrieve data from the ENA browser REST URL }
\usage{
ena(ids, portal, subtree = TRUE, limit = 1000, display = "xml") }
\arguments{
  \item{ids}{  EMBL accessions or taxonomy ID }
  \item{portal}{ taxonomy portal name }
  \item{subtree}{ return all subtree records }
  \item{limit}{   number of records to return }
  \item{display}{  display options (xml, fasta or text), default xml }
}
\details{  If portal is missing, then records matching one or more EMBL accessions are returned in either XML, FASTA, or plain text formats depending on the display option.  EMBL accessions can be a vector, comma-separated list, range, or single ID.  

If portal is specified, then records associated with a single taxonomy ID or name are returned.  Valid portal names include study, sample, analysis, read_sample, read_experiment, read_run, sequence_coding, sequence_release and others described on the help page listed in references.  
}
\value{ A DNAStringSet object if display="fasta", an XMLInternalDocument if display="xml", or a character vector.
}
\references{ The ENA browser REST services are described on the help page at \url{http://www.ebi.ac.uk/ena/about/browser}
}
\author{ Chris Stubben
}
 \note{This function retrieves data using \code{http://www.ebi.ac.uk/ena/data/view } and does not search the new data warehouse described on the help page }


\seealso{ \code{\link{enaParse}} }
\examples{
\dontrun{
ena("A00145")
ena("A00145,A00146", "fasta")


# Taxonomy portal: use name (requires lookup at NCBI) or ID
# ena("Coxiella burnetii", "sample")
x <-ena(777, "sample")
enaParse( x )

}


}
\keyword{ methods}
