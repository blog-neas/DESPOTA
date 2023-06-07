#' despotaPar
#' 
#' @description Despota parallelized
#' 
#' @param i asd
#' @param matrixNodes asd
#'
#' @export 
#'
#' @keywords internal

findpos <- function(i, matrixNodes) { 
  
  if(i == nrow(matrixNodes)) return(i)
  
  as.numeric(names(which(matrixNodes[i : nrow(matrixNodes), 1] == 1)[ matrixNodes[i, 1]])) 
  
}
