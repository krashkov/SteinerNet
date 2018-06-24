simulation_tests <- function (type, repeattimes, optimize, glist, ter_list) {
    
        g <- glist[[1]]

        result_simulations <- c()
    
        for (i in seq_along(ter_list)) {
        
                if (length(ter_list[[i]]) >= length(V(g)))
                        stop("Number of terminals is greater than number of vertices in graph")
        
                runtime <- 0
                a       <- Sys.time()
                steiner_graph <- steinertree(type = type, repeattimes = repeattimes, optimize = optimize, terminals = ter_list[[i]],
                                             graph = g, color = FALSE, merge = FALSE)
                b       <- Sys.time()
                runtime <- b - a
            
                time_type <- units(runtime)
                runtime   <- as.numeric(runtime)

                if (time_type == "mins" ) runtime <- runtime * 60
                if (time_type == "hours") runtime <- runtime * 3600
                if (time_type == "days" ) runtime <- runtime * 86400
        
                edges_num <- length(E(steiner_graph[[1]]))
                vert_num  <- length(V(steiner_graph[[1]]))
                trees_num <- length(steiner_graph)
                term_freq <- length(ter_list[[i]]) / length(V(steiner_graph[[1]]))
                edge_dens <- 2 * edges_num / ( vert_num * (vert_num - 1) )
                ter_num   <- length(ter_list[[i]])
        
                result_var        <- c( runtime,   edges_num,   vert_num,   trees_num,   term_freq,   edge_dens,   ter_num)
                names(result_var) <- c("runtime", "edges_num", "vert_num", "trees_num", "term_freq", "edge_dens", "ter_num")
        
                result_simulations[[i]] <- result_var
    }
    
    return(result_simulations)
}



####------------------------------------------ Documentation ------------------------------------####
#' Execute Steiner Algorithms and calculate parameters of output trees
#' 
#' @description This function executes given Steiner Algorithms and calculates such parameters as 
#'              runtime of each algorithm, number of edges, number of vertices, terminal frequency, edge density of
#'              resultant Steiner tree and number of trees in case of "EXA" or "SPM" algorithm. Wraps steinertree function.
#' 
#' @usage steiner_simulation(type, repeattimes = 70, optimize = TRUE, graph, ter_list)
#' 
#' @param type a character vector, which indicates types of algorithms to perform. Can be
#'             "EXA", "SP", "KB", "RSP", "SPM" or "ASP".
#' @param repeattimes a numeric scalar to specify "RSP" algorithm; number of times the optimization procedure is repeated.
#' @param optimize a logical scalar to specify all algorithms in type variable (ignored for "EXA"); if TRUE, an optimization
#'                 of the resultant steiner tree is performed, otherwise nothing is done.
#' @param graph an igraph graph; should be undirected, otherwise it is converted to undirected.
#' @param ter_list a list each element of which contains a numeric or character vector with ids of terminals or
#'                 a character vector with names of vertices (only if vertices have name attribute). Therefore, length of
#'                 ter_list declares a number of experiments to perform with different terminal sets
#'                 for each type of algorithm.
#' 
#' @return List, each element of which corresponds to specific algorithms type. Each element of a list also
#'         is a list and contains a named vector of evaluated parameters of steiner tree
#'         (runtime, edges_num, vert_num, trees_num, term_freq, edge_den). Number of sublist corresponds to
#'         number of terminal set in ter_list.
#'         
#' @details As a ter_list, a vector can be passed. In this case it is converted to a list containing only one element.
#' 
#' @seealso \code{\link{generate_st_samples}}, \code{\link{steinertree}}
#' 
#' @examples
#' library(igraph)
#' 
#' g <- graph("Cubical")
#' 
#' steiner_simulation(type = c("SP", "KB", "SPM"),
#'                    graph = g,
#'                    ter_list = generate_st_samples(graph = g,
#'                                                   ter_number = c(2, 3),
#'                                                   prob = c(0.1, 0.2)))
#'                    
#' steiner_simulation(type = c("EXA", "RSP"),
#'                    graph = g,
#'                    ter_list = c(1, 3, 8))
#' 
#' @references 1. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network
#'                identification: an empirical study", BMC Bioinformatics 2013 14:144
#'                
#' @export
####------------------------------------- End Documentation -------------------------------------####
steiner_simulation <- function (type, repeattimes = 70, optimize = TRUE, graph, ter_list) {
    
        glist  <- c()
        result <- c()
    
        glist[[1]] <- as.undirected(graph)
    
        type_list <- c("EXA", "SP", "KB", "RSP", "SPM", "ASP")
    
        if (sum(type %in% type_list) != length(type))
                stop("Some or all values in type variable are invalid")
    
        if (typeof(ter_list) != "list") {
                buf           <- ter_list
                ter_list      <- c()
                ter_list[[1]] <- buf
        }
    
        for (i in type)
                result[[i]] <- simulation_tests(type = i, repeattimes, optimize = optimize, glist = glist, ter_list = ter_list)
    
        return(result)
}
