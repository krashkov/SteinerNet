
\name{generate_st_samples}
\alias{generate_st_samples}
\alias{generate_st_samples, character, igraph object, character, list, list}
\alias{generate_st_samples, character, igraph object, missing, list, list}

\title{generate_st_samples}
\description{This function generates simulation data. It creates random graphs with randomly selected terminals.}
\usage{
generate_st_samples(test, graph, folder= NULL, listofterminaltest, repetition)
}

\arguments{
  \item{test}{\code{test} selects the test type to make random data for it. the random walk for exact algorithm makes subgraphs that include random terminals, but for heuristics it selects random terminals on the base graph and returns the terminal set only}
  \item{graph}{\code{graph} is the base graph for generating random subgraphs and random terminal set.}
  \item{folder}{\code{folder} specifies a folder name to store the simulated data inside it.}
  \item{listofterminaltest}{\code{listofterminaltest} is an input list. Elements of the list are number of terminals to select for a simulation.}
  \item{repetition}{\code{repetition} is a list of probabilites. Its length declares the number of elements if random data set that is created for terminals. Each element of the list is the probability of selecting a node as terminal while the random walk is traversing the base graph.}
}

\details{
This function generates random data for two type of simulations. For experiments that include exact algorithms, it generates random subgraphs with randomly selected terminals. Otherwise it returns only a set of random terminals to be used with the base graph.

Test specifies the type of simulation. 
\code{test} can be exact or appr,

	\code{exact} refers to generation of data for an experiment that includes exact Steiner tree algorithm.
	
	\code{appr} refers to generation of data for a experiment that involves only approximate Steiner tree algorithms.
	
If \code{folder} is NULL, it will use default value "steinerdata2" for folder when type is \code{exact} and "steinerdata" when type is \code{appr}.

\code{listofterminaltest} in our study was made of {5, 8, 20, 50, 70} for comparing approximate algorithms and it was {5, 8} for experiments that included exact Steiner tree algorithm. [1]

In our study, we repeated the tests 50 times, and we made the random walk to select a node to be terminal with 0.5 probability. Therefore \code{repetition} in our comparison was a list of fifty 0.5 values.
}

\value{
The function stores the random data in address that is stated in \code{folder}. 
When \code{test} is exact the output includes random subgraphs and random set of terminals.
When \code{test} is appr the function returns random sets of terminals.
}


\references{
	1. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network identification: an empirical study", BMC Bioinformatics 2013 14:144  
}

\author{Afshin Sadeghi}

\examples{
       library(igraph)
       library(SteinerNet)
       g <- igraph::graph.ring(10)
       \dontshow{
                library(igraph)
                library(SteinerNet)
                g <- igraph::graph.ring(10)
       }
       \donttest{
      generate_st_samples("exact", g, "testfolder", c(2,3), c(.8,.8))
       }
}

\keyword{graphs, protein interaction, network, pathway data graph, steiner tree}



