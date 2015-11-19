\name{rank.abund.pd}
\alias{rank.abund.pd}
\title{Cumulative PD across rank abundance distribution}
\description{Quantifies Faiths PD index beginning with the 2 most common species in a community and recalculating as increasingly rare species are added.}
\usage{rank.abund.pd(my.sample, my.phylo)}
\arguments{
  \item{my.sample}{A community data matrix}
  \item{my.phylo}{A community phylogeny}
}

\value{


	\bold{ rank.abund.pd }
	The PD value first for the 2 most common species and then for the 3 most common until the rarest species is added.

}
\references{Mi, X., N.G. Swenson, R. Valencia, W.J. Kress, D.L. Erickson, A. Perez-Castaneda, H. Ren, S.H. Su, N. Gunatilleke, S. Gunatilleke, Z. Hao, W. Ye, M. Cao, H.S. Suresh, H.S. Dattaraja, R. Sukumar, and K. Ma. 2012. The contribution of rare species to community phylogenetic diversity across a global network of forest plots. The American Naturalist 180:E17-E30.
}
\author{ Nathan G. Swenson }
\examples{
data(lefse.phylo)
data(lefse.sample)
rank.abund.pd(lefse.sample,lefse.phylo)
}
