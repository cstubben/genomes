\name{sra}
\alias{sra}
\docType{data}
\title{ Microbial SRA samples at the ENA }
\description{ Next-generation sequencing projects from microbes in the Sequence Read Archive (SRA)
at the European Nucleotide Archive (ENA). 
}
\usage{data(sra)}
\format{
  A data frame with 17161 observations on the following 123 variables.
  \describe{
    \item{\code{taxid}}{ taxonomy id}
    \item{\code{name}}{ scientific name (if missing, then title) }
    \item{\code{alias}}{ name qualifier from alias attribute}
    \item{\code{sample}}{ SRA sample }
    \item{\code{submission}}{SRA submission}
    \item{\code{study}}{ SRA study}
    \item{\code{experiment}}{SRA experiment}
    \item{\code{center}}{sequencing center}
    \item{\code{bases}}{ number of bases}
    \item{\code{reads}}{ number of reads}
    \item{\code{submit}}{ submission date}
    \item{\code{model}}{ model of sequencer }
    \item{\code{type}}{ study type }
  }
}
\details{ Downloaded from ENA on Oct 3, 2011.  Created by joining \code{enaSRA("Bacteria")} and 
 \code{enaSRA("Archaea")} and adding submission dates using \code{\link{enaSubmission}}, model 
  using \code{\link{enaExperiment}} and study type using \code{\link{enaStudy}}.  Microbes represent ~6\% of the total bases in the SRA.
}
\source{ SRA sample portal at ENA }
%\references{ }
\examples{
data(sra)

table2(species( sra$name))
table2(sra$center)
table2(sra$model)
table2(sra$study)

#Average read lengths by model
data.frame(read=round(tapply(sra$bases/sra$reads, list(sra$model ), mean, na.rm=TRUE), 1))

# image plot by model and year
y <- tapply(sra$bases, list(sra$model, year( sra$submit ) ), sum, na.rm=TRUE)
image2( y / 1e9, mar=c(1,11, 4,1) , log=TRUE, round=1)
title("Total microbial bases submitted per year (billions)", cex.main=1, line=2)






}
\keyword{datasets}
