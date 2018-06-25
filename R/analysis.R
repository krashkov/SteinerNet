####--------------------------------------- Documentation ---------------------------------------####
#' Plot simulated data
#' 
#' @description This function plots the comparison results of simulated data ans stores it in PDF file.
#' 
#' @usage steiner_comparison_plots(type, method, data, outputname)
#' 
#' @param type a character vector, which indicates types of algorithms to perform. Can be
#'             "EXA", "SP", "KB", "RSP", "SPM" or "ASP".
#' @param method a character vector; specifies a calculated parameter based on which comparison is performed. Can be 
#'               "runtime" (for time of execution), "log10runtime", "edge" (for number of edges in resultant steiner tree),
#'               "log10edge", "ter_freq" (for terminal frequency in resultant steiner tree) or "edge_dens" (for edge density
#'               in resultant steiner tree).
#' @param data should have structure as output of steiner_simulation function.
#' @param outputname a character scalar; name of file in which the result is stored
#' 
#' @return For each method a plot with comparison of algorithms (pointed in type variable) is created.
#'         An additional information about the number of experiments and number of terminals
#'         for each type of algorithm is added. If the number of terminals is the same for each type,
#'         then their number is printed, otherwise the range is printed.
#' 
#' @seealso \code{\link{generate_st_samples}}, \code{\link{steiner_simulation}}, \code{\link{steinertree}}
#' 
#' @examples
#' g <- graph("Cubical")
#' 
#' data <- steiner_simulation(type = c("SP", "KB", "SPM"),
#'                            graph = g,
#'                            ter_list = generate_st_samples(graph = g,
#'                                                           ter_number = c(2, 3),
#'                                                           prob = c(0.1, 0.2)))
#' 
#' steiner_comparison_plots(type = c("SP", "KB"),
#'                          method = c("runtime", "ter_freq"),
#'                          data = data,
#'                          outputname = "plot1.pdf")
#' 
#' @references 1. Afshin Sadeghi and Holger Froehlich, "Steiner tree methods for optimal sub-network
#'                identification: an empirical study", BMC Bioinformatics 2013 14:144
#'                
#' @export
####------------------------------------- End Documentation -------------------------------------####
steiner_comparison_plots <- function (type, method, data, outputname) {
        
        type_list <- c("EXA", "SP", "KB", "RSP", "SPM", "ASP")
        if (sum(type %in% type_list) != length(type))
                stop("Some or all values in type variable are invalid")
    
        method_list <- c("runtime", "log10runtime", "edge", "log10edge", "ter_freq", "edge_dens")
        if (sum(method %in% method_list) != length(method))
                stop("Some or all values in method variable are invalid")
    
        if (!is.character(outputname) | is.na(outputname))
                stop("outputname is invalid")
    
        pdf(outputname, paper = "a4")
        par(mar = c(5 + length(type), 4, 2, 2))
    
        var_list  <- c()
        type_list <- c()
        
        for (k in seq_along(method)) {
                
                for (i in seq_along(type)) {
                        
                        if (is.null(data[[type[i]]]))
                                stop("Error: type not found in data")
                        
                        for (j in seq_along(data[[type[1]]])) {
                                
                                if (method[k] == "runtime")
                                        var_list[i]  <- data[[type[i]]][[j]]["runtime"]
                                
                                if (method[k] == "log10runtime")
                                        var_list[i]  <- log10(data[[type[i]]][[j]]["runtime"])
                                
                                if (method[k] == "edge")
                                        var_list[i]  <- data[[type[i]]][[j]]["edges_num"]
                                
                                if (method[k] == "log10edge")
                                        var_list[i]  <- log10(data[[type[i]]][[j]]["edges_num"])
                                
                                if (method[k] == "ter_freq")
                                        var_list[i]  <- data[[type[i]]][[j]]["term_freq"]
                                
                                if (method[k] == "edge_dens")
                                        var_list[i]  <- data[[type[i]]][[j]]["edge_dens"]
                                
                                type_list[i] <- type[i]
                        }
                }
                
                if (method[k] == "runtime")
                        ylab <- c("Time of execution")
                
                if (method[k] == "log10runtime")
                        ylab <- c("Log10 of time of execution")
                
                if (method[k] == "edge")
                        ylab <- c("Number of edges")
                
                if (method[k] == "log10edge")
                        ylab <- c("Log10 of number of edges")
                
                if (method[k] == "ter_freq")
                        ylab <- c("Terminal frequency")
                
                if (method[k] == "edge_dens")
                        ylab <- c("Edge density")
                
                sizeplot <- data.frame(var = var_list, type = type_list)
                
                boxplot(var ~ type,
                        data = sizeplot,
                        col  = "lightgray",
                        ylab = ylab)
                
                for (z in seq_along(type)) {
                        num_of_exp  <- data[[type[z]]]
                        extract_num <- lapply(data[[type[z]]], function (x) x)
                        num_of_ter  <- unlist(lapply(extract_num, function(x) x["ter_num"]))
                        
                        if (min(num_of_ter) == max(num_of_ter)) {
                                mtext(paste(type[z], "   ",
                                            "Num. of experiments: ", length(num_of_exp), 
                                            "   Num. of terminals (|S|):", num_of_ter[1],
                                            sep = ""),
                                      side = 1, line = 4 + z, adj = 1)
                        } else {
                                mtext(paste(type[z], "   ",
                                            "Num. of experiments: ", length(num_of_exp), 
                                            "   Num. of terminals lies within ",
                                            "[", min(num_of_ter), ", ", max(num_of_ter), "]",
                                            sep = ""),
                                      side = 1, line = 4 + z, adj = 1)
                        }
                                
                }
        }
    
        dev.off()
}
