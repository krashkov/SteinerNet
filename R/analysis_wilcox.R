####------------------------------------- Documentation -------------------------------------####
#' Perform wilcox test
#' 
#' @description Perform pairwise Wilcoxon rank sum tests
#' 
#' @usage steiner_comparison_wilcox(type, method, data)
#' 
#' @param type a character vector, which indicates type of algorithms to perform. Can be
#'             "EXA", "SP", "KB", "RSP", "SPM" or "ASP".
#' @param method a character scalar; specifies a calculated parameter based on which comparison is performed. Can be 
#'               "runtime" (for time of execution), "log10runtime" "edge" (for number of edges in resultant steiner tree),
#'               "log10edge", "ter_freq" (for terminal frequency in resultant steiner tree) or 
#'               "edge_dens" (for edge density in resultant steiner tree).
#' @param data should have structure as output of steiner_simulation function.
#' 
#' @return Object of class "pairwise.htest"
#' 
#' @seealso \code{\link{generate_st_samples}}, \code{\link{steiner_simulation}},
#'          \code{\link{steinertree}}, \code{pairwise.wilcox.test}
#' 
#' @details "holm" method for adjusting p-values is used.
#' 
#' @examples
#' library(igraph)
#' 
#' g <- graph("Cubical")
#' 
#' data <- steiner_simulation(type = c("SP", "KB", "SPM"),
#'                            graph = g,
#'                            ter_list = generate_st_samples(graph = g,
#'                                                           ter_number = c(2, 3),
#'                                                           prob = c(0.1, 0.2)))
#' 
#' steiner_comparison_wilcox(type = c("SP", "KB"),
#'                           method = "ter_freq",
#'                           data = data)
#' 
#' @references 1. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network
#'                identification: an empirical study", BMC Bioinformatics 2013 14:144
#'                
#' @export
####------------------------------------- End Documentation -------------------------------------####
steiner_comparison_wilcox <- function (type, method, data) {
    
        type_list <- c("EXA", "SP", "KB", "RSP", "SPM", "ASP")
        if (sum(type %in% type_list) != length(type))
                stop("Some or all values in type variable are invalid")
    
        method_list <- c("runtime", "log10runtime", "edge", "log10edge", "ter_freq", "edge_dens")
        if (length(method) != 1)
                stop("Length of method doesn't equal to 1")
        else
                if (!(method %in% method_list))
                        stop("Invalid method")
    
        var_list  <- c()
        type_list <- c()
    
        for (i in seq_along(type)) {
                
                if (is.null(data[[type[i]]]))
                        stop("Error: type not found in data")
                
                for (j in seq_along(data[[type[1]]])) {
                        if (method == "runtime")
                                var_list [length(var_list) + 1] <- data[[type[i]]][[j]]["runtime"]
                        
                        if (method == "log10runtime")
                                var_list [length(var_list) + 1] <- log10(data[[type[i]]][[j]]["runtime"])
                        
                        if (method == "edge")
                                var_list [length(var_list) + 1] <- data[[type[i]]][[j]]["edges_num"]
            
                        if (method == "log10edge")
                                var_list [length(var_list) + 1] <- log10(data[[type[i]]][[j]]["edges_num"])
            
                        if (method == "ter_freq")
                                var_list [length(var_list) + 1] <- data[[type[i]]][[j]]["term_freq"]
            
                        if (method == "edge_dens")
                                var_list [length(var_list) + 1] <- data[[type[i]]][[j]]["edge_dens"]
                        
                        type_list[length(type_list) + 1] <- type[i]
                }
        }
        
        pairwise.wilcox.test(x = var_list, g = type_list, p.adjust.method = "holm", paired = TRUE)
}
