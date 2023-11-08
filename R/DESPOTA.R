#' despota
#' 
#' @description DESPOTA hierarchical clustering of a dataset or a dissimilarity matrix
#' 
#' @param data a matrix or data.frame of data 
#' @param distmat a dissimilarity matrix obtained through the `dist()` function
#' @param distMethod distance method to use, one of "euclidean", "maximum", 
#' "manhattan", "canberra", "binary" or "minkowski" (default "euclidean")
#' @param agglMethod agglomerative method to use, one of "ward.D", "ward.D2", 
#' "single", "complete", "average", "mcquitty", "median"or "centroid"
#'  (default "ward.D2")
#' @param M number of permutations (default 999)
#' @param alpha significant level associated to the testing procedure
#' (default 0.01)
#' @param listVal logical to add additional information regarding the splitting 
#' procedure (default FALSE)
#' @param seed seed for the permutation
#' @param par logical to perform parallel computing (default TRUE)
#' @param ncores number of cores to be used. Default `ncores = NULL` automatically 
#' chooses the number of all possible physical cores minus one.
#' @details
#' Frontend function that fits a dendrogram and finds the optimal cut through the DESPOTA procedure. 
#' 
#' @return A list of two elements, `Arguments` and `DF`. The first contains the 
#' dendrogram branches and splits in a dendextend format and additional information 
#' of the method used; the second object is a data.frame containing the observations, 
#' the cluster labels and their related p-values.
#' When `listVal = TRUE`, the output has an additional object containing the info 
#' regarding test statistics in each split.
#' 
#' @export
#' 
#' @examples 
#' data <- c(1,2,3,4,5, 7,9,10,11,12,  19,24,28,32,38, 54)
#' despota(data, distMethod = "euclidean", agglMethod = "ward.D2", 
#' M = 999, alpha = 0.05, seed = 1234, par = FALSE)
#' 
despota <- function(data, distmat = NULL, distMethod = "euclidean", 
                    agglMethod = "ward.D2", M = 999, alpha = 0.01, 
                    listVal = FALSE, seed = NULL, par=TRUE, ncores = NULL) {
  
  if(par==TRUE)
  {
    output <- despotaPar(data, distmat, distMethod, 
               agglMethod , M, alpha, 
               listVal, seed, ncores)
  }else{
  output <- despotaBase(data, distmat, distMethod, 
              agglMethod , M, alpha, 
              listVal, seed)
  }
  class(output) <- "despota"
 return(output)
}
