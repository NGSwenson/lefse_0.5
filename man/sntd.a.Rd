\name{sntd.a}
\alias{sntd.a}
\title{  Functional standard deviation of the NND with abundance weighting }
\description{ Quantifies the functional standard deviation of the nearest neighbor distance with abundance weighting }
\usage{ sntd.a(dist.mat, my.sample) }
\arguments{
  \item{dist.mat}{ A trait distance matrix. }
  \item{my.sample}{ A community data matrix. }
}
\details{ Quantifies the functional standard deviation of the nearest neighbor distance with abundance weighting }

\value{


	\bold{ sntd.a }
	The abundance weighted standard deviation of the nearest functional neighbor distance for each community


}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{Fsntd}} }

\examples{

data(lefse.sample)
data(lefse.traits)

sntd.a(as.matrix(dist(lefse.traits)), lefse.sample)


}
