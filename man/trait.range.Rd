\name{trait.range}
\alias{trait.range}
\title{ Range of trait values in a community }
\description{ Quantifies the range of trait values present in a community }
\usage{ trait.range(my.sample, traits) }
\arguments{
  \item{my.sample}{ A community data.matrix. }
  \item{traits}{ A trait matrix. }
}
\details{ Quantifies the the range of trait values in each community. }

\value{


	\bold{ trait.range }
	The range of trait values for each community.


}



\examples{
data(lefse.sample)
data(lefse.traits)

trait.range(lefse.sample, lefse.traits)



}
