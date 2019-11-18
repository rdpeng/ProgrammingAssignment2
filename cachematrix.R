#Week 3 - Assignment: Caching the Inverse of a Matrix
#Step 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#In order to assign a value to an object in an environment that is different from the current one, we use the <<- operator
#We start by taking the matrix as an input, and setting the value of the Matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
#We continue to get the value of the Matrix
get <- function() x                              
#Now we set and get the value of the invertible Matrix
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#Step 2 - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#We start by returning a matrix which is the inverse of 'x', by considering that inverse matrix is not Null
cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("Cached Invertible")
return(inv)
}
#Then we continue by returning a matrix which is the inverse of 'x', by considering that the value of the invertible matrix is Null
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv
}
