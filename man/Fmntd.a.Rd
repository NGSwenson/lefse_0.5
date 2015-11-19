\name{Fmntd.a}
\alias{Fmntd.a}
\title{ Functional NND with abundance }
\description{ Quantifies the functional mean nearest neighbor distance with abundance weighting }
\usage{ Fmntd.a(dist.mat, my.sample) }
\arguments{
  \item{dist.mat}{ A trait distance matrix. }
  \item{my.sample}{ A community data matrix. }
}
\details{ Quantifies the functional mean nearest neighbor distance with abundance weighting }

\value{


	\bold{ Fntda.a }
	The abundance weighted mean nearest functional neighbor distance for each community


}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{Fmntd}} }

\examples{
data(lefse.sample)
data(lefse.traits)

Fmntd.a(as.matrix(dist(lefse.traits)), lefse.sample)



}
