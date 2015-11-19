\name{lefse.sample}
\alias{lefse.sample}
\docType{data}
\title{ Community data matrix for lefse
}
\description{ Community data matrix for lefse package. Matrix contains 10 species and 5 communities }
\usage{data("lefse.sample")}
\format{
  A data frame with 5 observations on the following 10 variables.
}
\source{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }

\examples{
data(lefse.sample)
lefse.sample
rowSums(lefse.sample)
}

