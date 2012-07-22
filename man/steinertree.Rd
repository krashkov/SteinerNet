
\name{steinertree}
\alias{steinertree}
\alias{steinertree,boolean,boolean,list,boolean,graph}
\alias{steinertree,charahter,missing,list,boolean,graph}
\alias{steinertree,missing,character,list,boolean,graph}
\alias{steinertree,missing,missing,missing,boolean,graph}
\alias{steinertree,missing,boolean,list,boolean,graph}
\alias{steinertree,missing,missing,missing,graph}
\alias{steinertree,missing,missing,boolean,graph}

\title{steinertree}
\description{A function which involves a set of steiner tree algorithms on networks. This set involves an exact algorithm and five heuristic algorithms.}
\usage{
steinertree(type, coloring = FALSE, ter_list = NULL, enumerate = FALSE, graph)
}

\arguments{
  \item{type}{\code{type} specifies which steiner algorithm to use.}
  \item{coloring}{\code{coloring} is a boolean value. When it is TRUE, the function will return a list that consists of the result stiener tree and a copy of the input graph while including the random terminals within the input graph are specified by coloring of nodes. In this graph, green nodes would represent steiner nodes and red nodes represent terminals.}
  \item{ter_list}{\code{ter_list} is an input list of terminals. Steiner tree algorithm finds the result according to terminals.}
   \item{enumerate}{\code{enumerate} a boolean value to force the steiner tree function to return merged enumerated solutions.}
  \item{graph}{\code{graph} is an input igraph object which is delivered to one of the steiner tree algorithms of the package.} 
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

According to our knowledge RSP, EXA Enumeration, SPM are represented for the first time in this package and are new algorithms. [4]

\code{ter_list} value can be NULL. In this case, the function will observe graph vertex colors to find terminals. To call the function in this approach, the terminal nodes should be colored in red and the non-terminal nodes should be yellow.

This function handles input igraph objects which their vertices have labels and names. To apply the function on graphs with no 
label and name, \code{steinertree} function  automatically recognizes non-labeled graph vertices and creates names and labels for them. The new labels and names for vertices are created according to the vertice ID of each vertice.

}

\value{
When \code{coloring} is FALSE returns a Steiner tree in form of a new igraph object.
When \code{coloring} is TRUE returns a list that consists of two objects. The first is a steiner tree and the second object is a colored version of the input graph with distinguished steiner nodes and terminals.
}


\references{
	1. Path heuristic and Original path heuristic ,Section 4.1.3 of the book "The Steiner tree Problem", Petter,L,Hammer
	
	2. "An approximate solution for the Steiner problem in graphs" , H Takahashi, A Matsuyama
	
	3. F K. Hwang, D S. Richards and P Winter,"The steiner tree Problem", Kruskal-Based Heuristic Section 4.1.4,ISBN: 978-0-444-89098-6

	4. Please refer to the paper that is pulished with this package. 

	5. F K. Hwang, D S. Richards and P Winter,"The steiner tree Problem", Kruskal-Based Heuristic Section 4.1.4,
The Optimal solution for stiner trees on networks,ISBN: 978-0-444-89098-6.
}

\author{Afshin Sadeghi, Holger Froehlich}

\examples{
 #example 1
 library(SteinerNet)
 g <- graph.ring(10)
 ter_list= c(1,2,9)	
 SPM=steinertree("SPM", FALSE, ter_list, TRUE, g)
 
 #example 2
 g2 <- graph( c(0,2,1,2,2,3,3,4,5,6,3,6,2,7,2,5,2,6,5,8), directed=FALSE)
 V(g2)$color="yellow"
 V(g2)$color[c(1,4)]="red"		
 OP=steinertree("SP", TRUE, NULL, FALSE, g2)

 #example 3: A case study with a sample graph and a given gene list

 g <- graph( c(0,2,1,2,2,3,3,4,5,6,3,6,2,7,2,5,2,6,5,8), directed=FALSE)
 V(g)$name=c(1058, 51203, 6515, 83879, 160897, 10531, 8659, 2947, 643008)
 geneid_list= c(1058,83879, 160897,643008)
 list =as.character(geneid_list)
 #we include into the test those geneIDes who exist within the base graph.
 r =1:(length(list)) 	 
 t =sapply (r ,function(r) !is.na(match(list[r],c(g)[[9]][[3]]$name ) ))
 IDlist1=list[t==TRUE]
 ST1=steinertree( "SP", FALSE,IDlist1, FALSE ,g) 
 #result1= graphEnt_Sym(ST1[[1]])  # converts the Gene entrez ID taken from the graph node "names" and store them as "labels" of the nodes 
 #tkplot(result1)             # tkplot function displays labels instead of names

}
\keyword{graphs, protein interaction, network, pathway data graph, steiner tree}



