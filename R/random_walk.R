gRandWalk <- function (glist, numofterminals, terminalpr) {
    
        nodlist <- c()
        g <- glist[[1]]

        seed <- sample(1:length(V(g)), 1)
    
        i <- 1

        while (i < numofterminals) {

                seed <- sample(unlist(neighborhood(g, 1, nodes = seed, mode = "all")), 1)
                prob <- runif(1, min = 0, max = 1)

                if (prob < terminalpr) {
                        nodlist <- c(nodlist, V(g)[seed])
                        nodlist <- unique(nodlist)
                        i <- length(nodlist)
                }
        }
    
        return(nodlist)
}



####--------------------------------------- Documentation ---------------------------------------####
#' Select terminals
#' 
#' @description Provides random walk procedure. Starting from the randomly selected node, choose a neighbour node uniformly
#'              randomly, until given number of terminals won't be found.
#' 
#' @usage generate_st_samples(graph, ter_number, prob)
#' 
#' @param graph an igraph graph; should be undirected, otherwise it is converted to undirected.
#' @param ter_number a numeric vector; each element indicates the number of terminals to be selected and length of vector
#'                   indicates the number of terminal sets to be picked.
#' @param prob a numeric vector of the same length as ter_number; prob[i] defines a probability with which each
#'             next node accepted or rejected while selecting ter_number[i] terminals.
#' 
#' @return A list of the same length as ter_number. Each element of list contains a vector of ids of selected vertices.
#' 
#' @examples
#' library(igraph)
#' 
#' generate_st_samples(graph = graph("Zachary"),
#'                     ter_number = c(3, 4),
#'                     prob = c(0.1, 0.2))
#' 
#' @references 1. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network
#'                identification: an empirical study", BMC Bioinformatics 2013 14:144
#'                
#' @export
####------------------------------------- End Documentation -------------------------------------####
generate_st_samples <- function (graph, ter_number, prob) {

        ter_list <- c()
        glist    <- c()

        glist[[1]] <- as.undirected(graph)

        if (length(ter_number) != length(prob))
                stop("The length of ter_number is not equal to the length of prob")

        for (i in 1:length(ter_number)) {
                
                if (ter_number[i] >= length(V(glist[[1]])))
                        stop("Number of terminals to be selected is greater than number of vertices in graph")

                if ( !((prob[i] > 0) & (prob[i] <= 1)) )
                        stop("prob doesn't belong to (0, 1]")

                ter_list[[i]] <- gRandWalk(glist = glist, numofterminals = ter_number[i],
                                           terminalpr = prob[i])
        }
    
        return(ter_list)
}
