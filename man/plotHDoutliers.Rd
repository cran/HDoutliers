\name{plotHDoutliers}
\alias{plotHDoutliers}
\title{
   Display Outlier Detection Results
}
\description{
Plotting function outliers detected
}

\usage{
plotHDoutliers(data, indexes = NULL, ...) 
}

\arguments{
  \item{data}{
    A vector, matrix, or data frame consisting of numeric and/or categorical
    variables.
  }
  \item{indexes}{
    The (row) indexes of the outliers in \code{data}.
  }
  \item{...}{
    Additional plotting arguments.
  }
}

\details{
   Produces a plot of the data showing the outliers. If the data has more than
   two dimensions, it is plotted onto the principal components of the data that
   remains after removing outliers.
}

\value{
  The indexes of the observations determined to be outliers.
}

\section{References}{
  Wilkinson, L. (2016). Visualizing Outliers.
}

\seealso{
  \code{\link{HDoutliers}},
}

\examples{

data(dots)
out.W <- HDoutliers(dots$W)
\dontrun{
plotHDoutliers(dots$W,out.W)}

data(ex2D)
out.ex2D <- HDoutliers(ex2D)
\dontrun{
plotHDoutliers(ex2D,out.ex2D)}

}

\keyword{cluster}
