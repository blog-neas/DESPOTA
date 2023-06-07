test_that("run a simple DESPOTA routine using the parallelized function", {
    data <- c(1,2,3,4,5, 7,9,10,11,12,  19,24,28,32,38, 54)
    ncor <- ifelse(parallel::detectCores(logical = FALSE) > 1,2,1)
    testDP <- despota(data, distMethod = "euclidean",
    agglMethod = "ward.D2", M = 999, alpha = 0.05, seed = 1234, par = TRUE, ncores = ncor)
})
