\name{new.comdistnn}
\alias{new.comdistnn}
\title{Quantifies the mean nearest functional neighbor between communities without abundance weighting}
\description{The mean nearest functional neighbor between each pair of communities, Dnn, is calculated without abundance weighting. }
\usage{
new.comdistnn(my.sample, my.dist.mat)
}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.dist.mat}{A square trait distance matrix not a dist object}
}

\value{


	\bold{ new.comdistnn }
	The presence-absence weighted mean nearest functional neighbor distance between species in each of the two communities.
}
\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\examples{

data(lefse.sample)
data(lefse.traits)
mat = as.matrix(dist(lefse.traits))
new.comdistnn(lefse.sample, mat)
}
