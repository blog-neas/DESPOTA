% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DESPOTApar.R
\name{despotaPar}
\alias{despotaPar}
\title{despotaPar}
\usage{
despotaPar(
  data,
  distmat = NULL,
  distMethod = "euclidean",
  agglMethod = "ward.D2",
  M = 999,
  alpha = 0.01,
  listVal = FALSE,
  seed = NULL,
  ncores = NULL
)
}
\arguments{
\item{data}{a matrix or data.frame of data}

\item{distmat}{a dissimilarity matrix obtained through the `dist()` function}

\item{distMethod}{distance method to use, one of "euclidean", "maximum", 
"manhattan", "canberra", "binary" or "minkowski" (default "euclidean")}

\item{agglMethod}{agglomerative method to use, one of "ward.D", "ward.D2", 
"single", "complete", "average", "mcquitty", "median"or "centroid"
 (default "ward.D2")}

\item{M}{number of permutations (default 999)}

\item{alpha}{significant level associated to the testing procedure
(default 0.01)}

\item{listVal}{logical to add additional information regarding the splitting 
procedure (default FALSE)}

\item{seed}{seed for the permutation}

\item{ncores}{number of cores to be used. Default `ncores = NULL` automatically chooses the number of all possible physical cores minus one.}
}
\value{
a list
}
\description{
Despota parallelized
}
\keyword{internal}
