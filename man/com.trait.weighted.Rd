\name{com.trait.weighted}
\alias{com.trait.weighted}
\title{ Quantifies the moments of the community trait distribution weighted by abundance }
\description{ Quantifies the moments of the community trait distribution weighted by abundance }
\usage{ com.trait.weighted(my.sample, traits) }
\arguments{
  \item{my.sample}{ A community data matrix.}
  \item{traits}{ A trait file with species names as row names and trait as columns. }
}
\details{ Quantifies the moments of the community trait distribution weighted by abundance }
\value{


	\bold{ com.weighted.mean } The abundance weighted mean trait value in the community.


	\bold{ com.weighted.sd } The abundance weighted standard deviation of trait values in the community.

}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{com.trait.moments}} }
\examples{



data(lefse.sample)
data(lefse.traits)
com.trait.weighted(lefse.sample, lefse.traits)



}
