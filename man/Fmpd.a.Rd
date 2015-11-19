\name{Fmpd.a}
\alias{Fmpd.a}
\title{ Functional MPD with abundance }
\description{ Quantifies the functional mean pairwise distance with abundance weighting }
\usage{ Fmpd.a(dist.mat, my.sample) }
\arguments{
  \item{dist.mat}{ A trait distance matrix. }
  \item{my.sample}{ A community data matrix. }
}
\details{ Quantifies the functional mean pairwise distance with abundance weighting }

\value{


	\bold{ Fntda.a }
	The abundance weighted mean pairwise distance for each community


}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{Fmpd}} }

\examples{

data(lefse.sample)
data(lefse.traits)

Fmpd.a(as.matrix(dist(lefse.traits)), lefse.sample)


}
