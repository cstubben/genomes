\name{table2}
\alias{table2}

\title{ Format and sort a contigency table  }
\description{  
   Formats the output of \code{\link{table}} into an matrix ordered by
   total counts in descending order
}
\usage{
table2(\dots, n = 10)
}
\arguments{
  \item{\dots}{ one or more objects passed to \code{\link{table}} }
  \item{n}{ number of rows to display, default 10 }
}
\details{Currently limited to 1 or 2 dimensional table arrays. }
\value{
A matrix, sorted by total counts in descending order. Any
rows or columns with zero counts are also removed from the matrix.}

%\references{  }
\author{Chris Stubben  }
%\note{ }
\seealso{ \code{\link{table}}  }
\examples{
data(leuks)
table(leuks$subgroup)
table2(leuks$subgroup)
## to display all rows, use NA or a large number...
table2(leuks$subgroup, n=100)
# 2-d table
table2(leuks$group, format(leuks$released, "\%Y"))
}
\keyword{ methods }
