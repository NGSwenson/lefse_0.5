\name{Fmpd}
\alias{Fmpd}
\title{ Functional MPD no with abundance }
\description{ Quantifies the functional mean pairwise distance with no abundance weighting }
\usage{ Fmpd(dist.mat, my.sample) }
\arguments{
  \item{dist.mat}{ A trait distance matrix. }
  \item{my.sample}{ A community data matrix. }
}
\details{ Quantifies the functional mean pairwise distance without abundance weighting }

\value{


	\bold{ Fntda.a }
	The presence-absence weighted mean pairwise distance for each community


}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{Fmpd.a}} }

\examples{
data(lefse.sample)
data(lefse.traits)

Fmpd(as.matrix(dist(lefse.traits)), lefse.sample)

}
