\name{weighted.faith}
\alias{weighted.faith}
\title{ Abundance weighted calculation of Faiths PD index}
\description{Quantifying Faiths index of phylogenetic diversity weighting species contributions by their abundance}
\usage{weighted.faith(my.phylo, my.sample)}
\arguments{
  \item{my.phylo}{A phylo}
  \item{my.sample}{A community data matrix}
}
\value{
	\bold{}
	The abundance weighted Faiths index value.
}
\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }

\examples{
data(lefse.sample)
data(lefse.phylo)
weighted.faith(lefse.phylo,lefse.sample)
}