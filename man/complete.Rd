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
  A data frame with 1716 observations on the following 10 variables.
  \describe{
    \item{\code{pid}}{genome project id}
    \item{\code{name}}{taxonomy name}
    \item{\code{released}}{release date in \code{lproks} table }
    \item{\code{genbank}}{genbank ID of the largest chromosome from
        the comma-separated list in the \code{lproks} table}
    \item{\code{pmid}}{ first pubmed ID from the comma-separated list
        in the \code{lproks} table. Warning: may not be the genome
        sequencing paper }
    \item{\code{history}}{ the revision history date associated with the
        genbank ID -  if replaced sequences are found, then common
        revision history was used }
    \item{\code{published}}{ the published date of the first pubmed ID}
    \item{\code{wgs_acc}}{the assembly accession }
    \item{\code{wgs_date}}{the assembly release date}
    \item{\code{source}}{ likely source of the \code{lproks} release
        date }
  }
}
\details{ This table was created to check release dates
  in the \code{\link{lproks}} table.
  Currently, 170 genomes are mislabeled with the assembly release date
  (out of 465 that were previously released as an assembly) and many
  of the first genomes released report "published'' dates.
}
\source{ The \code{lproks} table is at  \url{http://www.ncbi.nlm.nih.gov/genomes/lproks.cgi}.
See \url{http://www.ncbi.nlm.nih.gov/projects/WGS/WGSprojectlist.cgi }
for a list of assembly projects superceded by a complete genome sequence.
}
% \references{ }
\examples{
data(complete)
# Note: early genomes use published dates and many are incorrect
#  Synechocystis sp. PCC 6803 uses published date from sequencing 1 Mb region paper
#  Streptomyces uses published date from ordered cosmid library paper
complete[1:5, ]
# source of release dates - 203 do no match history, published or wgs date
table(complete$source)
# genomes previously submitted as WGS
table(is.na(complete$wgs_acc))
}
\keyword{datasets}
