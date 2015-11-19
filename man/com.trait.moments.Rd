\name{com.trait.moments}
\alias{com.trait.moments}
\title{com.trait.moments}
\description{ Quantifies the moments of the community trait distribution}
\usage{ com.trait.moments(my.sample, traits) }
\arguments{
  \item{my.sample}{ A community data matrix. }
  \item{traits}{ A trait file with species names as row names and trait as columns. }
}

\details{ Quantifies the moments of the community trait distribution }
\value{


	\bold{ com.mean }  The mean trait value of the community
	
	
	\bold{ com.sd }  The standard deviation of the trait values in the community


	\bold{ com.skew }  The skewness of the trait values in the community


	\bold{ com.kurt }  The kurtosis of the trait values in the community
}

\references{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\author{ Nathan G. Swenson }
\seealso{ \code{\link{com.trait.weighted}} }
\examples{

data(lefse.sample)
data(lefse.traits)
com.trait.moments(lefse.sample, lefse.traits)



}
