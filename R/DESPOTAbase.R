#' despotaBase
#' 
#' @description Despota Base
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
#' @param verbose additional information of the procedure(default FALSE)
#'
#' @return a list 
#' @import dendextend fpc fastcluster
#' @importFrom stats as.dendrogram as.dist dist order.dendrogram
#' @importFrom utils tail
#' @export 
#'
#' @keywords internal
#' 

despotaBase <- function(data, distmat = NULL, distMethod = "euclidean", 
                        agglMethod = "ward.D2", M = 999, alpha = 0.01, 
                        listVal = FALSE, seed = NULL, verbose=FALSE) {
  
  date1 <- format(Sys.time(), "%a %d %b %Y, %X")
  if(verbose) cat("Call:  method = ",agglMethod,", M = ",M,", alpha = ",
                  alpha,"\n",sep = "")
  
  stopifnot(alpha > 0 & alpha < 1 & M > 0)
  
  if(agglMethod == "ward") {
    
    warning("Method 'ward' is now called 'ward.D'")
    agglMethod <- "ward.D"
  }
  
  pmat <- pmatch(agglMethod, possib.aggl <- c("ward.D", "ward.D2", "single", 
                                              "complete", "average", "mcquitty", "median", "centroid"))
  
  if(is.na(pmat))
    stop("The possible linkages are: 'ward.D', 'ward.D2', 'single',
         'complete', 'average', 'mcquitty', 'median', 'centroid'")
  
  agglMethod <- possib.aggl[pmat]
  
  if(!is.null(seed)) set.seed(seed)
  
  if(!is.null(distmat)) {distMeasure <- distmat; attr(distMeasure, "method") <- "unknown"
  data <- 'unknown data'} 
  else {
    
    if(!(is.na(pmatch(distMethod, "squared euclidean"))))
    {distMeasure <- stats::dist(data)^2; attr(distMeasure, "method") <- "squared euclidean"} 
    else {distMeasure <- stats::dist(data, method = distMethod)}
  }
  
  distMatrix <- as.matrix(distMeasure)
  
  Hcd <- stats::as.dendrogram(fastcluster::hclust(distMeasure, method = agglMethod))
  
  #suppressMessages(library(dendextend))
  
  matrixNodes <- sapply(c("members", "height"), dendextend::get_nodes_attr, dend = Hcd)
  
  rownames(matrixNodes) <- 1:nrow(matrixNodes)
  
  listClust <- as.list(numeric(nrow(matrixNodes)))
  
  lab <- stats::order.dendrogram(Hcd)
  
  for(i in 1: nrow(matrixNodes)) {
    
    listClust[[i]] <- lab[1 : matrixNodes[i, "members"]]
    
    if(matrixNodes[i, "members"] == 1) lab <- lab[-1]
    
  }
  
  names(listClust) <- 1:nrow(matrixNodes)
  
  ####!!! Qua c'era la function findpos, adesso è in utils
  
  x <- sapply(1: nrow(matrixNodes), findpos, matrixNodes = matrixNodes)
  
  STRING <- rep("0", nrow(matrixNodes))
  
  for(i in 2: nrow(matrixNodes)) {
    
    STRING[i: x[i]] <- ifelse(matrixNodes[i - 1, 1] == 1, 
                              paste0(STRING[i: x[i]], "2"), 
                              paste0(STRING[i: x[i]], "1"))   
    
  }
  
  matrixNodes <- data.frame(matrixNodes, code = STRING, stringsAsFactors = F)
  
  ordmatr <- with(matrixNodes, order(- height, - members))
  
  MAT <- matrixNodes[ordmatr, ]
  
  LIS <- listClust[ordmatr]
  
  limit <- which(MAT$members == 1)
  
  MAT$Leftheight[limit] <- 0
  MAT$Rightheight[limit] <- 0
  MAT$membLeft[limit] <- 0
  MAT$membRight[limit] <- 0
  
  Clusters <- list()
  length(Clusters) <- length(limit) - 1
  
  
  for(i in 1 : (limit[1] - 1)) {
    
    cod <- MAT$code[i]
    
    strinL <- paste0(cod, "1")
    strinR <- paste0(cod, "2")
    
    whi <- match(c(strinL, strinR), MAT$code)
    
    MAT$Leftheight[i] <- MAT[ whi[1], ]$height
    MAT$Rightheight[i] <- MAT[ whi[2], ]$height
    
    MAT$membLeft[i] <- MAT[ whi[1], ]$members
    MAT$membRight[i] <- MAT[ whi[2], ]$members
    
    Clusters[[i]]$Left <- sort(unlist(LIS[ whi[1] ], use.names = F))
    Clusters[[i]]$Right <- sort(unlist(LIS[ whi[2] ], use.names = F))
    
  }
  
  cluste <- 1
  
  clustest <- rep(NA, nrow(MAT))
  pvalues <- rep(NA, nrow(MAT))
  
  
  lh <- MAT$Leftheight[1]
  rh <- MAT$Rightheight[1]
  
  if(lh == 0) {
    
    cod0 <- paste0(MAT$code[1], "1")
    
    strin0 <- substr(MAT$code, 1, nchar(cod0))
    
    whi0 <- which(cod0 == strin0)
    
    clustest[ whi0 ] <- paste0("C", cluste)
    
    cluste <- cluste + 1
    
    pvalues[ whi0 ] <- NA
    
  } 
  
  if(rh == 0) {
    
    cod0 <- paste0(MAT$code[1], "2")
    
    strin0 <- substr(MAT$code, 1, nchar(cod0))
    
    whi0 <- which(cod0 == strin0)
    
    clustest[ whi0 ] <- paste0("C", cluste)
    
    cluste <- cluste + 1
    
    pvalues[ whi0 ] <- NA
    
  }
  
  list_values <- list()
  rck <- NA
  rcm <- NA
  
  for(i in 2:nrow(MAT)) {
    
    if(is.na(clustest[i])) {
      
      lh <- MAT$Leftheight[i]
      rh <- MAT$Rightheight[i]
      
      if(!(lh == 0 & rh == 0) | MAT$membLeft[i] > 1 | MAT$membRight[i] > 1) {
        
        rck <- with(MAT[i, ], abs(Leftheight - Rightheight) / (height - min(c(Leftheight, Rightheight))))
        
        rcm <- numeric(M)
        
        lobs <- MAT$membLeft[i]
        robs <- MAT$membRight[i]
        
        ClusterLeft <- Clusters[[i]][[1]]
        ClusterRight <- Clusters[[i]][[2]]
        
        distMat <- as.matrix(stats::as.dist(distMatrix[ LIS[[i]], LIS[[i]] ]))
        
        start <- 1
        
        while(start <= M){
          
          for(j in start: M) {
            
            samT <- sample(1: MAT$members[i], replace = FALSE)
            samL <- samT[1: lobs]
            samR <- samT[lobs + 1: robs]
            
            if((lh == 0 | rh == 0) & !(lobs > 1 & robs > 1)) {
              
              if(lobs == 1) { 
                
                distRightClusterPerm <- stats::as.dist(distMat[samR, samR])
                hcij <- fastcluster::hclust(distRightClusterPerm, method = agglMethod)
                obsij <- ClusterRight; obsk <- ClusterLeft; heightk <- 0
                
              } else { 
                
                distLeftClusterPerm <- stats::as.dist(distMat[samL, samL])
                hcij <- fastcluster::hclust(distLeftClusterPerm, method = agglMethod)
                obsk <- ClusterRight; obsij <- ClusterLeft; heightk <- 0 
              }
              
              heightij <- utils::tail(hcij$height, 1)
              
            } else {
              
              distLeftClusterPerm <- stats::as.dist(distMat[samL, samL])
              distRightClusterPerm <- stats::as.dist(distMat[samR, samR])
              
              hcL <- fastcluster::hclust(distLeftClusterPerm, method = agglMethod)
              hcR <- fastcluster::hclust(distRightClusterPerm, method = agglMethod)
              
              heiL <- utils::tail(hcL$height, 1)
              heiR <- utils::tail(hcR$height, 1)
              
              ifelse(heiL <= heiR,
                     { hcij <- hcR; heightij <- heiR; heightk <- heiL 
                     obsij <- ClusterRight; obsk <- ClusterLeft },
                     { hcij <- hcL; heightij <- heiL; heightk <- heiR
                     obsk <- ClusterRight; obsij <- ClusterLeft })
              
            }
            
            nk <- length(obsk)
            
            splij <- split(obsij, cutree(hcij, 2))
            
            ni <- length(splij[[1]])
            
            nj <- length(splij[[2]])
            
            distik <- stats::as.dist(distMatrix[c(obsk, splij[[1]]), c(obsk, splij[[1]])])
            heightik <- utils::tail(fastcluster::hclust(distik, method = agglMethod)$height, 1)
            
            distjk <- stats::as.dist(distMatrix[c(obsk, splij[[2]]), c(obsk, splij[[2]])])
            heightjk <- utils::tail(fastcluster::hclust(distjk, method = agglMethod)$height, 1)
            
            heiN <- switch(agglMethod, 
                           
                           ward.D = ((ni + nk) * heightik + (nj + nk) * heightjk - 
                                       nk * heightij)/(ni + nj + nk),
                           
                           ward.D2 = sqrt(((ni + nk) * heightik^2 + (nj + nk) * heightjk^2 - 
                                             nk * heightij^2)/(ni + nj + nk)),
                           
                           single = min(heightik, heightjk),
                           
                           complete = max(heightik, heightjk),
                           
                           average = (ni * heightik + nj * heightjk)/(ni + nj),
                           
                           mcquitty = (heightik + heightjk)/2,
                           
                           median = (heightik + heightjk)/2 - heightij/4,
                           
                           centroid = (ni * heightik + nj * heightjk)/(ni + nj) - 
                             ni*nj * heightij/(ni + nj)^2)
            
            rcm[j] <- (heightij - heightk)/((heiN - heightk) + 0.00001)
            
          }
          
          finite_val <- is.finite(rcm)
          
          rcm <- rcm[finite_val]
          
          start <- 1 + sum(finite_val)
          
        }
        
        pval <- (sum((rcm <= rck)) +1)/(M +1)
        
        if(pval > alpha) {
          
          cod <- MAT$code[i]
          
          strin <- substr(MAT$code, 1, nchar(cod))
          
          clustest[which(cod == strin)] <- paste0("C", cluste)
          
          pvalues[which(cod == strin)] <- pval
          
          cluste <- cluste + 1
          
        } else {
          
          if(lh == 0) {
            
            cod0 <- paste0(MAT$code[i], "1")
            
            strin0 <- substr(MAT$code, 1, nchar(cod0))
            
            whi0 <- which(cod0 == strin0)
            
            clustest[ whi0 ] <- paste0("C", cluste)
            
            cluste <- cluste + 1
            
            pvalues[ whi0 ] <- pval
            
          } 
          
          if(rh == 0) {
            
            cod0 <- paste0(MAT$code[i], "2")
            
            strin0 <- substr(MAT$code, 1, nchar(cod0))
            
            whi0 <- which(cod0 == strin0)
            
            clustest[ whi0 ] <- paste0("C", cluste)
            
            cluste <- cluste + 1
            
            pvalues[ whi0 ] <- pval
            
          } 
          
        }
        
      } else {
        
        cod <- MAT$code[i]
        
        strin <- substr(MAT$code, 1, nchar(cod))
        
        clustest[which(cod == strin)] <- paste0("C", cluste)
        
        cluste <- cluste + 1
        
      }
    }
    
    list_values[[i]] <- list(rck = rck, rcm = rcm) 
    
    rck <- rcm <- NULL
    
  }
  
  DF <- data.frame(observation = utils::tail(unlist(LIS), n = length(limit)), 
                   cluster = clustest[limit], p.value = pvalues[limit])
  rownames(DF) <- rownames(distMatrix)[ DF$observation ]
  
  DF$cluster <- ordered(DF$cluster, levels = unique(DF$cluster))
  
  if(nlevels(DF$cluster) == 2) {
    
    message("DESPOTA alone is not able to choose between 1 or 2 clusters, default to 2. 
          Please, visualize the dendrogram plot.")
    
    if(!is.null(distmat)) {
      
      warning("If you want to get helped by the duda index, re-run DESPOTA providing
            the data and not the distance matrix")
      
    } else {
      
       cat("\nThe 'Duda and Hart' test suggests", 
          ifelse(fpc::dudahart2(data, DF$cluster, alpha)$cluster1, 
                 "1 cluster.\n\n", "2 clusters.\n\n"))    
      
    }
    
  }
  
  if(verbose) cat("Start: ", date1, "\nEnd: ", format(Sys.time(), "%X"), "\n\n")
  
  Args <- list(data = deparse(substitute(data)), Hcd = Hcd, 
               distMeasure = attr(distMeasure, "method"), 
               agglMethod = agglMethod, alpha = alpha, M = M, seed = seed)
  
  if(listVal) return(list(Arguments = Args, list_values = list_values, DF = DF))
  
  list(Arguments = Args, DF = DF) 
  
}
