\name{hmp}
\Rdversion{1.1}
\alias{hmp}
\docType{data}
\title{ Human Microbiome Project (HMP) data }
\description{
 Genome sequencing projects listed by the Human microbiome project
}
\usage{data(hmp)}
\format{
  A genomes data frame with  18 columns.
  \describe{
    \item{\code{pid}}{HMP project ID}
    \item{\code{name}}{ project name}
    \item{\code{status}}{project status }
    \item{\code{released}}{date draft sequencing completed}
    \item{\code{entrezid}}{ Entrez genome project id}
    \item{\code{taxid}}{NCBI taxonomy id}
    \item{\code{phylum}}{phylum}
    \item{\code{site}}{primary body sample site }
    \item{\code{subsite}}{ body sample subsite }
    \item{\code{goal}}{finishing goal }
    \item{\code{submission}}{NCBI submission status }
    \item{\code{center}}{ sequencing center name }
    \item{\code{assembler}}{assembler}
    \item{\code{reads}}{total number of reads}
    \item{\code{coverage}}{coverage}
    \item{\code{contigs}}{ number of contigs }
    \item{\code{platform}}{sequencing platform}
    \item{\code{comment}}{comments}
  }
}
\details{see the HMP Project Catalog at http://www.hmpdacc.org/ .  The
full table has 34 columns and only 18 columns are displayed in this dataset }
\source{  http://durian.jgi-psf.org/~kliolios/HMP/hmp.xls }
%\references{}
\examples{
data(hmp)
hmp
 t(hmp[1,])
summary(hmp)
plotby(hmp, "site", main='Human microbiome project', lbty='n', log='y', lcex=.7)
plotby(hmp)
table(hmp$status, !is.na(hmp$released), 
dnn=list("status", "released?"))

}
\keyword{datasets}
