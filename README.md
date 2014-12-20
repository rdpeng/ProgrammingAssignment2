

In this program, I have created two functions, makeCacheMatrix(xm = matrix()) and cacheSolve(xm). The function makeCacheMatrix has four local functions setM(xm), getM(), setInvM(invM), and getInvM(). The functions setM and getM are for setting and getting input matrix xm. Functions setInvM and getInvM are for setting and getting the calcucated inverse matrix. The variable invM is free variable in makeCacheMatrix, with assignment operator <<-, it is resolved from the calling function of cacheSolve().

I have tested this code with the following calls:

    xl <- makeCacheMaxtric(matrix(1:4, 2,2))

    cacheSolve(xl)

the printout is:

[1] "in CacheSolve, xm = " [1] " In invM." [1] "In getM." [1] "In setInvM." [,1] [,2] [1,] -2 1.5 [2,] 1 -0.5

call cacheSolve(xl) second time - [1] "in CacheSolve, xm = " [1] " In invM." getting cached data [,1] [,2] [1,] -2 1.5 [2,] 1 -0.5

yl <-makeCacheMatrix()

    yl$setM(matrix(sample.int(10, size=9, replace=TRUE),3,3))

[1] "In setM." [1] "in setM" [,1] [,2] [,3] [1,] 3 4 10 [2,] 5 6 6 [3,] 10 2 8

    cacheSolve(yl) [1] "in CacheSolve, xm = " [1] " In invM." [1] "In getM." [1] "In setInvM." [,1] [,2] [,3] [1,] -0.11538462 0.03846154 0.115384615 [2,] -0.06410256 0.24358974 -0.102564103 [3,] 0.16025641 -0.10897436 0.006410256

call cacheSolve(yl) second time -

[1] "in CacheSolve, xm = " [1] " In invM." getting cached data [,1] [,2] [,3] [1,] -0.11538462 0.03846154 0.115384615 [2,] -0.06410256 0.24358974 -0.102564103 [3,] 0.16025641 -0.10897436 0.006410256

Thank you for reading.
