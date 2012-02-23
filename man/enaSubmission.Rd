\name{enaSubmission}
\alias{enaSubmission}
\title{ ENA SRA submission dates }
\description{ Return details about SRA submissions in the ENA }
\usage{
enaSubmission(accs, batchsize = 100)
}
\arguments{
  \item{accs}{ a vector of SRA submissions with prefix SRA, ERA, DRA, etc, or a range of accessions}
  \item{batchsize}{ number of accs to inlcude in a single comma-separated url string }
}
\details{ Parses the submission date attribute in the submission tag }
\value{ a data.frame with acc number, title and submitted date }
\references{\url{http://www.ebi.ac.uk/ena/about/browser#sra_xml} }
\author{ Chris Stubben}
%\note{ }


\seealso{ \code{\link{sra}} and \code{\link{enaSRA}}  }
\examples{
#compare to http://www.ebi.ac.uk/ena/data/view/ERA000746
enaSubmission("ERA000746")
# or ranges 
# enaSubmission("SRA000600-SRA000610")

# chimps
#pan<-enaSRA(9596) 
#head(pan)
#enaSubmission(pan$submission)

}
\keyword{ methods }
