\name{enaStudy}
\alias{enaStudy}
\title{ ENA SRA study details }
\description{ Return details about SRA studies in the ENA }
\usage{
enaStudy(accs, batchsize = 100)
}
\arguments{
 \item{accs}{  a vector of SRA studies or a range of accessions with prefix SRP, ERP, DRP, etc.}
 \item{batchsize}{number of accs to include in a single comma-separated url string }
}
%\details{}
\value{ a data.frame with study title, type, description, analysis  }
%\references{}
\author{ Chris Stubben }
\note{ only a few studies have secondary analysis }

\seealso{  \code{\link{enaSRA}}  }
\examples{
# compare to http://www.ebi.ac.uk/ena/data/view/ERP000054
enaStudy("ERP000054")

# chimps
pan<-enaSRA(9596) 
head(pan)
pan2 <-enaStudy(pan$study)
head( pan2,2)
pan2[,1:2]
table2(pan2$type)
}
\keyword{ methods }
