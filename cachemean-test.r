my_matrix <- makeCacheMatrix(matrix(rnorm(16),4,4))
my_matrix$get() %*% cacheSolve(my_matrix)
##[,1]          [,2]          [,3]          [,4]
##[1,]  1.000000e+00  0.000000e+00 -1.110223e-16 -5.551115e-17
##[2,]  2.775558e-17  1.000000e+00 -5.551115e-17  1.387779e-17
##[3,] -5.551115e-17 -1.110223e-16  1.000000e+00 -5.551115e-17
##[4,]  2.775558e-16  0.000000e+00  0.000000e+00  1.000000e+00