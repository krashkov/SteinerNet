
\name{steinertree}
\alias{steinertree}
\alias{steinertree, character list, graph, boolean, boolean}
\alias{steinertree, character list, graph}
\alias{steinertree, character list, graph, boolean}
\alias{steinertree, character list, graph, missing, boolean}
\alias{steinertree, NULL, graph, boolean, boolean}
\alias{steinertree, NULL, graph}
\alias{steinertree, NULL, graph, boolean}
\alias{steinertree, NULL, graph, missing, boolean}
\title{steinertree}
\description{A function which involves a set of steiner tree algorithms on networks. This set involves an exact algorithm and five heuristic algorithms.}
\usage{
steinertree(type, ter_list = NULL, graph, enumerate = FALSE, coloring = TRUE)
}

\arguments{
  \item{type}{\code{type} specifies which steiner algorithm to use.}
  \item{ter_list}{\code{ter_list} is an input list of terminals. This list should have character type. Steiner tree algorithm finds a solution according to this list.}
  \item{graph}{\code{graph} is an input igraph object which is delivered to one of the steiner tree algorithms of the package. This graph should be connected and it should have undirected edges.} 
  \item{enumerate}{\code{enumerate} a boolean value to specify EXA and SPM algorithms. It tells the steiner tree function to return a merged enumerated set of solutions.}
  \item{coloring}{\code{coloring} is a boolean value. When it is TRUE, the function will return two results in a list. The first member of this list represents resulted steiner tree within the input graph by coloring it. In this case the terminals are specified by red color and Steiner nodes are represented by green. Second member of the output list is a single Steiner tree which is represented with a new graph object. When \code{coloring} is FALSE the function returns the answer in form of a new single graph.}

}

\details{
This function withholds six steiner tree algorithms for networks.
type specifes the steiner algorithm to deploy to the input graph and terminal set. 
\code{type} can be SP, KB, RSP, EXA or SPM. The explanation of the abbreviations is listed below.

	\code{SP} refers to the shortest path heuristic algorithm. [1,2]
	
	\code{KB} exerts to Kruskal-Based Heuristic algorithm. [3]
	
	\code{RSP} exerts a random approximation algorithm developed by the package developers. [4]
	
	\code{EXA} in single mode uses an exact algorithm to return one of the optimal solutions of the problem. In enumerate mode, returns the merged enumerated solution. [4,5]

	\code{SPM} in single mode returns one of heuristic enumeration algorithm solutions for the problem. In enumerate mode, returns the merged enumerated solution.[4]


EXA and SPM algorithms can return a single solution or run in enumerating mode. The boolean value of enumerate specefies one of the two cases. If this value is FALSE they return one of their enumerated steiner solutions without merging it to other solutions. If it is TRUE they return the merged enumerated solutions of the steiner tree problem. 

According to our knowledge RSP, EXA Enumeration, SPM and KB are represented for the first time in this package and in the paper that comes with its. [4]

\code{ter_list} value can be NULL. In this case, the function will observe graph vertex colors to find terminals. To call the function in this approach, the terminal nodes should be colored in red and the non-terminal nodes should be yellow.

This function handles input igraph objects which their vertices have labels and names. To apply the function on graphs with no label and name, \code{steinertree} function  automatically recognizes non-labeled graph vertices and creates names and labels for them. The new labels and names for vertices are created according to the vertice ID of each vertice.

}

\value{
When \code{coloring} is FALSE returns a Steiner tree in form of a new igraph object.
When \code{coloring} is TRUE returns a list that consists of two objects. The first is a steiner tree and the second object is a colored version of the input graph with distinguished steiner nodes and terminals.
}


\references{
	 
	1. Path heuristic and Original path heuristic ,Section 4.1.3 of the book "The Steiner tree Problem", Petter,L,Hammer
	
	2. "An approximate solution for the Steiner problem in graphs" , H Takahashi, A Matsuyama
	
	3. F K. Hwang, D S. Richards and P Winter,"The steiner tree Problem", Kruskal-Based Heuristic Section 4.1.4,ISBN: 978-0-444-89098-6

  4. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network identification: an empirical study", BMC Bioinformatics 2013 14:144 doi:https://doi.org/10.1186/1471-2105-14-144. 
	
	5. F K. Hwang, D S. Richards and P Winter,"The steiner tree Problem", Kruskal-Based Heuristic Section 4.1.4,
The Optimal solution for stiner trees on networks,ISBN: 978-0-444-89098-6.
}

\author{Afshin Sadeghi}

\examples{
      # Example 1
      library(igraph)
      library(SteinerNet)
      el <- matrix( c("a", "b", "a", "c", "b", "d","d","e", "c", "b" ), nc = 2, byrow = TRUE)
     g1 =graph_from_edgelist(el)
     ter_list= c("a","b","e")
     SP=steinertree("SP", ter_list, g1, TRUE, FALSE)
     SP[[1]]

    # Example 2

    el <- matrix( c("a", "b", "a", "c", "b", "d","d","e", "c", "b" ), nc = 2, byrow = TRUE)
    g1 =graph_from_edgelist(el)
    ter_list= c("a","b","e")
    KB = steinertree("KB", ter_list, g1 , TRUE)
    KB[[1]]
    KB[[2]]

    # Example 3

    EXA = steinertree("EXA", ter_list, g1 , TRUE , FALSE)
    EXA[[1]]

    # Example 4

    EXA2 = steinertree("EXA", ter_list, g1 , TRUE , TRUE)
    EXA2[[2]]
       \dontshow{
                library(igraph)
                library(SteinerNet)
       }
       \donttest{
    # Example 5: A case study with a sample graph and a given gene list
    
    g <- graph( c(9,2,1,2,2,3,3,4,5,6,3,6,2,7,2,5,2,6,5,8), directed=FALSE)
    V(g)$name= c("1058", "51203", "6515", "83879", "160897",
    "10531", "8659", "2947", "643008")
    geneid_list= c("1058","83879", "160897","643")
    # We include into the test those geneIDes who exist within the base graph.
    r = 1:(length(geneid_list)) 	 
    t = sapply (r ,function(r) !is.na(match(geneid_list[r],V(g)$name )) )
    glist = geneid_list[t==TRUE]
    ST1= steinertree( "SP", glist, g) 
    tkplot(result1)             # tkplot function displays labels instead of names
       }
}




\keyword{graphs, protein interaction, network, pathway data graph, steiner tree}



