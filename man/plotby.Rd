\name{plotby}
\Rdversion{1.1}
\alias{plotby}
\title{Plot groups of genomes by release date}
\description{ Plots the cumulative number of genomes by released date
  for different groups of genomes  }
\usage{
plotby(x, groupby = "status", subset = NA, top = 5, 
labels = FALSE,  curdate=TRUE, abbrev = TRUE, flip = NA,
 legend = "topleft", lbty = "o", lcol = 1, ltitle = NULL, lcex = 1,
 lsort = TRUE, cex = 1, inset=0, ylim = NA, las = 1, lwd = 1, log = "", 
xlab = "Release Date", ylab = "Genomes", type='l',
col = c("blue", "red", "green3", "magenta", "yellow"), 
lty = 1:top, pch = c(15:18, 1:3), \dots)
}

\arguments{
  \item{x}{ a genomes data frame  }
  \item{groupby}{  a column name in the genomes table or a vector to group by}
  \item{subset}{ logical vector indicating rows to keep }
  \item{top}{ number of top groups to display  } 
  \item{labels}{ plot a single line with labeled points using genome
      \code{name} column  }
  \item{ curdate}{ include the current date on x-axis, if false, then
      default is range of release dates }
  \item{abbrev}{ abbreviated genome names }
  \item{flip}{ a number indicating where to flip labels from right to
      left, default is middle of plot }
  \item{legend}{ a legend keyword or vector of x,y coordinates,
      defaults to top-left corner. Use NA for no legend} 
  \item{lbty}{ legend box type}
  \item{lcol}{ number of columns in legend }
  \item{ltitle}{ legend title }
  \item{lcex}{ legend size expansion }
  \item{inset}{inset legend distances(s)  }
  \item{lsort}{ sort legend by decreasing order of genomes,
      default true}
  \item{cex}{ label size expansion }
  \item{ylim}{ y axis limits }
  \item{las}{  rotate axis labels }
  \item{lwd}{ line width}
  \item{log}{ log scale }
  \item{xlab}{ x axis label }
  \item{ylab}{ y axis label }
  \item{type}{ plot type}
  \item{col}{ line or point colors }
  \item{lty}{ line type }
  \item{pch}{ point type }
  \item{\dots}{ additional items passed to plot }
}
\details{
Two different plot types are available.  The default is to plot
multiple lines, one for each group (like \link{matplot}).  If
\code{ labels=TRUE}, then a single line is drawn with different labeled points for each
group.}
\value{ A plot of released dates by group}
%\references{ }
\author{ Chris Stubben }
%\note{  }

\seealso{ \code{\link{plot.genomes}}  }
\examples{
data(lproks)
# default group is status
plotby(lproks)
plotby(lproks, 'habitat', top=3)

## groupby can be a vector
plotby(lproks, genus(lproks$name), log='y', lcex=.7)
plotby(lproks, factor(lproks$pathogen \%in\% c("No"),
   labels=c("Pathogen", "Non-pathogen")), pathogen!="")

# OR plot labels
plotby(lproks, subset=name \%like\% 'Yersinia pestis*', labels=TRUE, cex=.7, lbty='n')

}

\keyword{ hplot }
