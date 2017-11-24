
\name{steiner_comparison_plots}
\alias{steiner_comparison_plots}
\alias{steiner_comparison_plots, character, character , character, list, list}
\alias{steiner_comparison_plots, character, character, missing, list, list}
\alias{steiner_comparison_plots, character, character, character, missing, missing}
\alias{steiner_comparison_plots, character, character, missing, missing, missing}
\alias{steiner_comparison_plots, character, missing, missing, missing, missing}

\title{steiner_comparison_plots}
\description{This function plots the comparison results of Steiner tree algorithms excutions on simulated data.}
\usage{
steiner_comparison_plots (test_name, test_folder = NULL, outputname = NULL,
listofterminaltest = NULL ,repetition = NULL)
}

\arguments{
  \item{test_name}{\code{test_name} selects the plot type to creat. 14 type of comparison are available to perform.}
  \item{test_folder}{\code{testfolder} specifies a folder name to read the result of steiner tree simulations.}
  \item{outputname}{\code{outputname} is name of a pdf file to store the result.}
  \item{listofterminaltest}{\code{listofterminaltest} is an input list. Elements of the list are number of terminals that are selected for a simulation.}
  \item{repetition}{\code{repetition} is a list of probabilities. Its length declares the number of elements if random data set that is created for each terminal number.}
}

\details{
This function creates 12 different comparison types and depicts them by plots. \code{test_name} specifies the type of comparison. 

\code{test_name} can be on of the following character values.

	\code{exact} refers to time and edge number comparison of Steiner tree algorithms including the exact algorithm.
	
	\code{appr} refers to time and edge number comparison of Steiner tree algorithms without the exact algorithm.

	\code{Enum} refers to time and edge number comparison of Steiner tree enumeration algorithms.

	\code{Enum-median-venn-node-edge} refers edge and node number comparison of subgraphs made by Steiner tree enumeration algorithms via Venn diagram.

	\code{org} refers to edge number comparison of random subgraphs that are made by simulations.

	\code{org-dens-e} refers to edge density comparison of random graphs that are made by random graph generator.

	\code{appr-vfreq} refers to vertex frequency comparison of Steiner tree algorithms without the exact algorithm.

	\code{exact-vfreq} refers to vertex frequency comparison of Steiner tree algorithms including the exact algorithm.

	\code{Enum-vfreq} refers to vertex frequency comparison of Steiner tree enumeration algorithms.

	\code{appr-density-e} refers to edge density comparison of steiner tree algorithms excluding the exact algorithm.

	\code{exact-density-e} refers to edge density comparison of steiner tree algorithms including the exact algorithm.

	\code{Enum-density-e} refers to edge density comparison of steiner tree enumeration algorithms.
	
If \code{testfolder} is NULL, it will use default value "steinerdatae" for folder when \code{test} is \code{exact} and "steinerdataenum" when \code{test} is \code{enum}.

When \code{outputname} is NULL, a default value would be used for output pdf file name with consideration of selected type.

\code{listofterminaltest} in our study was made of {5, 8, 20, 50, 70} for comparing approximate algorithms and it was {5, 8} for experiments that included exact Steiner tree algorithm. [1]

In our study, we repeated the tests 50 times, and we made the random walk to select a node to be terminal with 0.5 probability while it traverses the base graph. Therefore \code{repetition} in our comparison was a list of fifty 0.5 values. If \code{repetition} is NULL, the function regards the \code{repetition} and \code{listofterminaltest} values that were used in our study.
}

\value{
The function stores a resulted plot in a PDF file. 
}


\references{
	1. Please refer to the paper that is published with this package. 
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
       steiner_comparison_plots ("exact", "testfolder", c(2,3), c(.8,.8))
       }
}

\keyword{graphs, protein interaction, network, graph, steiner tree}



