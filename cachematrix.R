> A<- matrix(c(1,2,3,4),2,2) 
>A
[,1] [,2]
[1,]    1    3
[2,]    2    4
> A1<- makeCacheMatrix(B)
##This function creates a special "matrix" object that can cache its inverse
> cacheSolve(A1)
##inverse returned after computation
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(A1)
##inverse returned from cache
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
