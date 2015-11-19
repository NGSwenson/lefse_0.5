\name{rank.abund.fric}
\alias{rank.abund.fric}
\title{Cumulative Functional Richness (FRic) across rank abundance distribution}
\description{Quantifies Functional Richness (FRic) index beginning with the 3 most common species in a community and recalculating as increasingly rare species are added.}
\usage{rank.abund.fric(my.sample, my.traits)}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.traits}{A trait matrix with row names being species names}
}

\value{


	\bold{ rank.abund.fric }
	The FRic value first for the 3 most common species and then for the 3 most common until the rarest species is added.

}
\references{Mi, X., N.G. Swenson, R. Valencia, W.J. Kress, D.L. Erickson, A. Perez-Castaneda, H. Ren, S.H. Su, N. Gunatilleke, S. Gunatilleke, Z. Hao, W. Ye, M. Cao, H.S. Suresh, H.S. Dattaraja, R. Sukumar, and K. Ma. 2012. The contribution of rare species to community phylogenetic diversity across a global network of forest plots. The American Naturalist 180:E17-E30.
}
\author{ Nathan G. Swenson }
\examples{
data(lefse.traits)
data(lefse.sample)
rank.abund.fric(lefse.sample,lefse.traits)
}
