\name{Fmntd}
\alias{Fmntd}
\title{  Functional NND }
\description{ Quantifies the functional mean nearest neighbor distance with no abundance weighting }
\usage{ Fmntd(dist.mat, my.sample) }
\arguments{
  \item{dist.mat}{ A trait distance matrix. }
  \item{my.sample}{ A community data matrix. }
}
\details{ Quantifies the functional mean nearest neighbor distance without abundance weighting }

\value{


	\bold{ Fntda }
	The presence-absence weighted mean nearest functional neighbor distance for each community


}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{Fmntd.a}} }

\examples{

data(lefse.sample)
data(lefse.traits)

Fmntd(as.matrix(dist(lefse.traits)), lefse.sample)


}
