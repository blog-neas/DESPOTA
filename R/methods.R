#' plot.despota
#' 
#' @description Plot a despota class object
#' 
#' @param x a despota object
#' @param main plot title
#' @param sub plot subtitle
#' @param ... additional options passed to the plot function
#' 
#' @return a dendrogram with the output of the despota model 
#' @import dendextend
#' @importFrom graphics abline mtext par
#' @export 
#'
#' @examples
#' data <- c(1,2,3,4,5, 7,9,10,11,12, 19,24,28,32,38, 54)
#' out <- despota(data, alpha = .05, seed = 31, listVal = TRUE, par = FALSE)
#' plot(out)
plot.despota <- function(x, main = NULL, sub = NULL, ...) {
  
  # dots <- list(...)
  # main <- dots$main
  # sub <- dots$sub
  
  data <- x$Arguments$data
  Hcd <- x$Arguments$Hcd
  distMeasure <- x$Arguments$distMeasure
  agglMethod <- x$Arguments$agglMethod
  alpha <- x$Arguments$alpha
  M <- x$Arguments$M
  DF <- x$DF
  
  labels(Hcd) <- rep(NA, length(labels(Hcd)))
  
  Hcd <- dendextend::assign_values_to_leaves_edgePar(Hcd, value = DF$cluster, edgePar = "col")
  Hcd <- dendextend::assign_values_to_leaves_edgePar(Hcd, value = 3, edgePar = "lwd")
  
  par0mar <- graphics::par()$mar
  
  graphics::par(mar=c(3.1, 4.1, 3.6, 2.1))
  
  plot(Hcd, main = main, sub = NA, las = 1, cex.axis = .6, ...)
  
  if(is.null(main)) {
    
    graphics::mtext(paste0("DESPOTA: ", nlevels(DF$cluster), " clusters found at alpha = ", alpha),
                    side=3, line=1.8, cex=1.3)
    
  }
  
  tab <- table(DF$cluster)
  ytexts <- rep(c(-.7, -.3, .1), ceiling(length(tab)/3))
  length(ytexts) <- length(tab)
  pvs <- tapply(DF$p.value, DF$cluster, function(x) round(unique(x), 3))
  
  graphics::mtext(pvs, col = seq_len(nlevels(DF$cluster)), side=1, 
                  at = .75 + tab/2 + c(0, cumsum(tab[ - length(tab)])), cex=.6, font=2, line = ytexts)
  
  if(is.null(sub)) {
    
    graphics::mtext(paste0("Data: '", data, "'"),
                    side=3, line=.5, font=3)
    
    graphics::mtext(paste0("Distance: '", distMeasure, "'. Linkage: '", agglMethod,
                           "'. M = ", M), side=1, line=1.6, font=3)
    
  } else graphics::mtext(sub, side=1, line=1.5)
  
  
  graphics::abline(v = cumsum(tab[ - length(tab)]) +.5, lty = "dotted", col = "darkgrey")
  
  #par(mar=c(5.1, 4.1, 4.1, 2.1))
  graphics::par(mar = par0mar)
  
}