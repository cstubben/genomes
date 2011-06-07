\name{term2summary}
\alias{term2summary}
\title{ Retrieve genome summaries from NCBI  }
\description{
  Search the Entrez Genome Project or Genome database at NCBI  and
  retrieve a summary table using Entrez programming
  utilities (eUtils)
}
\usage{
term2summary(term, db = 'genomeprj', sortdate = FALSE, fulltable = FALSE)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms  }
  \item{db}{ Database to search, either genomeprj or genome }
   \item{sortdate}{ Sort the results by status and released date (default is by
        name)}
  \item{fulltable}{ Return all 20 E-summary fields for genomeprj or 12
      fields for genome.  }
}
\details{
Searches either genome database using the ESearch utility, returns document summary
pages using the ESummary utility, and then parses the XML fields using
the \code{XML} package.

If searching Genome Project, then a genomes data frame with 4 columns (project id, name,
  status, released date) is returned. If fulltable is
  TRUE, then all 20 fields are returned, plus extra rows for overview
  genome projects (type = Top level), RefSeq genomes (type = RefSeq), and plasmid genomes
  (type = Plasmid genome).  In many cases, recent assemblies will be
  listed on an overview page, a genome page (missing released date), and a
  RefSeq page (missing status).  

If searching Genomes, then a genomes data frame with 6 columns (acc,
name (defline), status, released, taxid, size) is returned, or all 12 columns if
fulltable is TRUE.

}
\value{ A  genomes data frame 
}
\references{A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}.


 }
\author{ Chris Stubben }
%\note{}
\seealso{ \code{\link{term2neighbor}}  }

\examples{
# Genomes sequenced at Los Alamos 
x <- term2summary( "Los Alamos AND Bacteria[ORGN]")
x
summary(x)
#  list of centers in lproks table are often incomplete
data(lproks)
summary(lproks, center \%like\% '*Los Alamos*')


## Taxonomy queries like genomes in Bacteroidetes phylum
x <- term2summary("Bacteroidetes[ORGN]")
x
plot(x, ylab = 'Bacteroidetes genomes')

}

\keyword{ methods}
