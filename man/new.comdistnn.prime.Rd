\name{new.comdistnn.prime}
\alias{new.comdistnn.prime}
\title{Quantifies the mean nearest functional neighbor between communities with abundance weighting}
\description{The mean nearest functional neighbor between each pair of communities, Dnn, is calculated with abundance weighting. }
\usage{
new.comdistnn.prime(my.sample, my.dist.mat)
}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.dist.mat}{A square trait distance matrix not a dist object}
}

\value{


	\bold{ new.comdistnn.prime }
	The abundance weighted mean nearest functional neighbor distance between species in each of the two communities.
}
\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\examples{

data(lefse.sample)
data(lefse.traits)
mat = as.matrix(dist(lefse.traits))
new.comdistnn.prime(lefse.sample, mat)
}
