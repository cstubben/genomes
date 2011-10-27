\name{complete}
\Rdversion{1.1}
\alias{complete}
\docType{data}
\title{
 Complete microbial genome dates
}
\description{
Dates associated with complete microbial genomes at NCBI
}
\usage{data(complete)}
\format{
  A data frame with 1787 observations on the following 11 variables.
  \describe{
    \item{\code{pid}}{genome project id}
    \item{\code{name}}{taxonomy name}
    \item{\code{released}}{release date in the \code{lproks} table }
    \item{\code{genbank}}{genbank ID of the largest chromosome from
        the comma-separated list in the \code{lproks} table}
    \item{\code{history}}{ the revision history date associated with the
        genbank ID  }
    \item{\code{submitted}}{ the submission date associated with the genbank ID}
    \item{\code{pmid}}{ pubmed ID of genome paper from the comma-separated list
        in the \code{lproks} table.  }
    \item{\code{published}}{ the published date of the pubmed ID}
    \item{\code{wgs}}{the WGS accession, if previously released as an assembly}
    \item{\code{assembled}}{the assembly release date}
    \item{\code{source}}{ likely source of the \code{lproks} release
        date }
  }
}
\details{ This table was created to check release dates in the \code{\link{lproks}} table.
  The revision history date was added using \code{\link{ncbiRelease}}, the submission date using 
  \code{\link{ncbiSubmit}}, and publication date using the \code{\link{pub}} dataset.
  Currently, 178 complete genomes are mislabeled with the assembly release date
  (out of 473 that were previously released as an assembly) , the source for 204 others is unknown.
  and many of the first genomes released report "published" dates. 
}
\source{  See \url{http://www.ncbi.nlm.nih.gov/projects/WGS/WGSprojectlist.cgi }
for a list of the 473 assembly projects superceded by a complete genome sequence.
}
% \references{ }
\examples{
data(complete)
# some early genomes use published dates from the wrong paper (eg, 2nd and 4th genomes below) 
complete[1:5, ]
# likely source of release dates 
table2(complete$source)
# genomes previously submitted as WGS
table(is.na(complete$wgs))
subset(complete, !is.na(wgs))[1:2,]
}
\keyword{datasets}
