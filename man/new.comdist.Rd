\name{new.comdist}
\alias{new.comdist}
\title{Quantifies the mean pairwise functional distance between communities without abundance weighting}
\description{The mean pairwise functional between each pair of communities, Dpw, is calculated without abundance weighting. }
\usage{
new.comdist(my.sample, my.dist.mat)
}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.dist.mat}{A square trait distance matrix not a dist object}
}

\value{


	\bold{ new.comdist }
	The presence-absence weighted mean pairwise functional distance between species in each of the two communities.
}
\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\examples{

data(lefse.sample)
data(lefse.traits)
mat = as.matrix(dist(lefse.traits))
new.comdist(lefse.sample, mat)
}
