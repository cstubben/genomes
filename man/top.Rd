\name{top}
\Rdversion{1.1}
\alias{top}
\title{  Find the most common values    }
\description{  Finds the most common values in a vector with repeating
elements.
}
\usage{
top(x, n = 10)
}
\arguments{
  \item{x}{ A vector with some repeating elements }
  \item{n}{  The number of top elements }
}
\details{\code{top} returns a logical vector indicating if the
  element is one of the most common values in the vector
}
\value{A logical vector indicating if the element is one of the top
  values. 
}
%\references{ }
\author{ Chris Stubben }
\note{ This will mostly be useful in conjunction with the \code{\link[base]{subset}} function.
}
\seealso{\code{\link{like}}
}
\examples{ 
x <- c("a", "b", "b", "c")
top(x, 1)
#top is a short cut for
x \%in\% names(sort( table(x), decreasing=TRUE))[1]

data(lproks)
x <- subset(lproks, status != 'In Progress' , c(name, status, released))
# get top 15 genera
x <- subset(x, top(genus(name), 15))
x$status[x$status == 'Assembly'] <- 'WGS'
y <- table(genus(x$name), x$status)
y <- cbind(y, Total=rowSums(y))
y <- y[order(y[ ,3]), ]    # order by total

dotplot(y ,  xlab=list("Number of genomes at NCBI",cex=.8),
        par.settings=list(superpose.symbol=list(pch=15:17)),
        auto.key=list(cex=.8, columns=3, between=.5,  between.columns=1))
}

\keyword{ methods }

