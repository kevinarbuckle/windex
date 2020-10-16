\name{RichnessYuleTestPrep}
\alias{RichnessYuleTestPrep}
\title{
Pair of functions to generate the inputs necessary to run richness.yule.test() in ape
}
\description{
The function richness.yule.test() in the package ape requires two inputs - a dataframe with species richness of pairs of sister lineages which differ in the presence of a binary trait of interest, and a vector of divergence times of each of those sister group pairs. The two functions here generate these input files: sisterRichness returns a dataframe containing two columns (species richness in sister lineages with and without the trait of interest) and rows representing different sister pairs, and sisterDivTime returns a vector of divergence times of those sister pairs.
}
\usage{
sisterRichness(tree, x)
sisterDivTime(tree, x)
}
\arguments{
  \item{tree}{
Phylogenetic tree of class 'phylo' containing node labels representing divergence times of each node.
}
  \item{x}{
 Named vector representing the binary trait (labelled as 0 and 1 for absence and presence respectively).
}
}

\value{
 
}
\references{
}
\author{
Kevin Arbuckle
}

\seealso{
\code{\link{richness.yule.test}}
}
\examples{
}
