#' despotaPar
#' 
#' @description Despota parallelized
#' 
#' @param i number of rows to be matched
#' @param matrixNodes data matrix with the necessary data
#'
#' @export 
#'
#' @keywords internal

findpos <- function(i, matrixNodes) { 
  
  if(i == nrow(matrixNodes)) return(i)
  
  as.numeric(names(which(matrixNodes[i : nrow(matrixNodes), 1] == 1)[ matrixNodes[i, 1]])) 
  
}
