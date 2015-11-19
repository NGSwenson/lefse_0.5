\name{new.comdistnn.prime.null}
\alias{new.comdistnn.prime.null}
\title{Quantifies a random mean nearest functional neighbor between communities with abundance weighting}
\description{A random mean nearest functional neighbor between each pair of communities, Dnn, is calculated with abundance weighting. }
\usage{
new.comdistnn.prime.null(my.sample, my.dist.mat)
}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.dist.mat}{A square trait distance matrix not a dist object}
}

\value{


	\bold{ new.comdistnn.prime.null }
	A random abundance weighted mean nearest functional neighbor distance between species in each of the two communities.
}
\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\examples{

data(lefse.sample)
data(lefse.traits)
mat = as.matrix(dist(lefse.traits))
new.comdistnn.prime.null(lefse.sample, mat)
replicate(2,new.comdistnn.prime.null(lefse.sample, mat))
}
