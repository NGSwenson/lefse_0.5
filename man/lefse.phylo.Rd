\name{lefse.phylo}
\alias{lefse.phylo}
\docType{data}
\title{ Community phylogeny for lefse
}
\description{ Community phylogeny for lefse package. Phylogeny contains 10 species.  }
\usage{data("lefse.phylo")}
\format{
  The format is:
List of 4
 $ edge       : int [1:18, 1:2] 11 12 13 14 14 13 12 15 15 16 ...
 $ Nnode      : int 9
 $ tip.label  : chr [1:10] "sp10" "sp5" "sp8" "sp2" ...
 $ edge.length: num [1:18] 0.8062 0.2151 0.055 0.0565 0.0565 ...
 - attr(*, "class")= chr "phylo"
 - attr(*, "order")= chr "cladewise"
}
\source{ Swenson, N.G. 2014. Functional and Phylogenetic Ecology in R. Springer UseR! Series, Springer, New York, New York, U.S.A. }


\examples{
data(lefse.phylo)
lefse.phylo
plot(lefse.phylo)
}
