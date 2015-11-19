\name{Fsntd}
\alias{Fsntd}
\title{  Functional standard deviation of the NND }
\description{ Quantifies the functional standard deviation of the nearest neighbor distance with no abundance weighting }
\usage{ Fsntd(dist.mat, my.sample) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist.mat}{ A trait distance matrix. }
  \item{my.sample}{ A community data matrix. }
}
\details{ Quantifies the functional standard deviation of the nearest neighbor distance without abundance weighting }

\value{


	\bold{ Fsntd }
	The presence-absence weighted standard deviation of the nearest functional neighbor distance for each community


}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{sntd.a}} }

\examples{
data(lefse.sample)
data(lefse.traits)

Fsntd(as.matrix(dist(lefse.traits)), lefse.sample)

}
