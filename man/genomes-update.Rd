\name{genomes-update}
\alias{genomes-update}
\alias{update.genomes}

\title{ Genome table updates }
\description{
   Generic function for updating genome tables.  
}
\usage{
\method{update}{genomes}(object, \dots)
}
\arguments{
 \item{object}{ a genomes data frame to update }
  \item{\dots}{ additional arguments are currently ignored }
}
\details{
\code{update} will retrieve the new genome table using the update
string in \code{attr(object, 'update')}.  The new table will replace the existing
version,  \emph{but not permanently}, since reloading the dataset using \code{data}
will restore the older version.  If you have write permission, one option is to use
\code{\link{system.file}} to replace the data set (see the example below).

}
\value{
 Returns the updated genome table and a count of the number of new
 IDs added and old IDs removed.   Old IDs are typically assembly
 genomes in NCBI tables that have been released as a single
 complete genome.
}
%\references{  }
\author{Chris Stubben }
%\note{ }
\seealso{\code{\link{genomes-summary}}, \code{\link{genomes-plot}} }
\examples{
\dontrun{data(proks)}
\dontrun{update(proks)}

# to replace the data set permanently
x <- system.file("data", "proks.rda", package="genomes")
x
\dontrun{save(proks, file=x)}

}
\keyword{ methods }
