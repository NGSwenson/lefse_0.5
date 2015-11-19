\name{lefse.traits}
\alias{lefse.traits}
\docType{data}
\title{ Example trait dataset for lefse }
\description{ Example trait dataset for lefse package. The trait matrix contains 10 species and 3 traits }
\usage{data("lefse.traits")}
\format{
  A data frame with 10 observations on the following 3 variables.

}
\source{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }
\examples{
data(lefse.traits)
lefse.traits
hist(lefse.traits[,1])
}

