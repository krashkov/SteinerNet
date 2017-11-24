
\name{steiner_simulation}
\alias{steiner_simulation}
\alias{steiner_simulation, list, list, character}
\alias{steiner_simulation, list, list, missing}

\title{steiner_simulation}
\description{This function executes Steiner algorithms on simulated data and stores their results into files.}
\usage{
steiner_simulation(test,listofterminaltest,repetition,testfolder = NULL)
}

\arguments{
  \item{test}{\code{test} selects the test type to apply the simulation. It can be exact, appr, or enum.}
  \item{listofterminaltest}{\code{listofterminaltest} is an input list. Elements of the list are number of terminals that are selected for a simulation.}
  \item{repetition}{\code{repetition} is a list of probabilities. Its length declares the number of elements if random data set that is created for each terminal number.}
  \item{testfolder}{\code{testfolder} specifies a folder name to read the simulated data from it and to store Steiner tree algorithms results inside it.}
}

\details{
This function performs three type of experiments. Test specifies the type of comparison to perform.

\code{test} can be exact or appr or enum,

	\code{exact} refers to executing the set of steiner tree algorithms including the exact algorithm.
	
	\code{appr} forces to executing the set of steiner tree algorithms without the exact algorithm.

	\code{enum} refers to to executing the set of steiner tree enumeration algorithms.
	
\code{listofterminaltest} in our study was made of {5, 8, 20, 50, 70} for comparing approximate algorithms and it was {5, 8} for experiments that included exact Steiner tree algorithm. [1]

If \code{testfolder} is NULL, it will use default value "steinerdata2" for folder when \code{test} is \code{exact} and "steinerdataenum" when \code{test} is \code{enum}.

In our study, we repeated the tests 50 times, and we made the random walk to select a node to be terminal with 0.5  probability. Therefore \code{repetition} in our comparison was a list of fifty 0.5 values. We also survied the behavior of the algorithms when selection probability was and 0.2
 and 0.8.
}

\value{
The function stores the result of execution of Steiner trees and the time of their executions in an address that is stated in \code{testfolder}. 
}


\references{
	1. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network identification: an empirical study", BMC Bioinformatics 2013 14:144.
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
                steiner_simulation("exact", "testfolder", c(2,3), c(.8,.8))
       }
}

\keyword{graphs, protein interaction, network, pathway data graph, steiner tree}



