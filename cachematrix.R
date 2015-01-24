# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv_matrix <- NULL
set <- function(y) {
x <<- y
inv_matrix <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv_matrix <<- inverse
getinverse <- function() inv_matrix
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
	inv_matrix <- x$getinverse()
if(!is.null(inv_matrix)) {
	message("getting cached data.")
	return(inv_matrix)
}
data <- x$get()
inv_matrix <- solve(data)
x$setinverse(inv_matrix)
inv_matrix
}

sample run:


> b1<-matrix(c(1,2,3,2,5,2,6,-3,1), nrow=3, ncol=3)
> b1
     [,1] [,2] [,3]
[1,]    1    2    6
[2,]    2    5   -3
[3,]    3    2    1
> matrix1 <- makeCacheMatrix(b1)
> cacheSolve(matrix1)
           [,1]        [,2]        [,3]
[1,] -0.1428571 -0.12987013  0.46753247
[2,]  0.1428571  0.22077922 -0.19480519
[3,]  0.1428571 -0.05194805 -0.01298701

> b2<-matrix(c(1,2,6,2,5,-3,3,2,1), nrow=3, ncol=3)
> matrix2 <- makeCacheMatrix(b2)
> cacheSolve(matrix2)
> cacheSolve(matrix2)
           [,1]       [,2]        [,3]
[1,] -0.1428571  0.1428571  0.14285714
[2,] -0.1298701  0.2207792 -0.05194805
[3,]  0.4675325 -0.1948052 -0.01298701
> cacheSolve(matrix2)
getting cached data.
           [,1]       [,2]        [,3]
[1,] -0.1428571  0.1428571  0.14285714
[2,] -0.1298701  0.2207792 -0.05194805
[3,]  0.4675325 -0.1948052 -0.01298701
> 
> cacheSolve(matrix1)
getting cached data.
           [,1]        [,2]        [,3]
[1,] -0.1428571 -0.12987013  0.46753247
[2,]  0.1428571  0.22077922 -0.19480519
[3,]  0.1428571 -0.05194805 -0.01298701
> 
