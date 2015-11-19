\name{new.comdist.prime}
\alias{new.comdist.prime}
\title{Quantifies the mean pairwise functional distance between communities with abundance weighting}
\description{The mean pairwise functional between each pair of communities, Dpw, is calculated with abundance weighting. }
\usage{
new.comdist.prime(my.sample, my.dist.mat)
}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.dist.mat}{A square trait distance matrix not a dist object}
}

\value{


	\bold{ new.comdist.prime }
	The presence-absence weighted mean pairwise functional distance between species in each of the two communities.
}
\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\examples{

data(lefse.sample)
data(lefse.traits)
mat = as.matrix(dist(lefse.traits))
new.comdist.prime(lefse.sample, mat)
}
