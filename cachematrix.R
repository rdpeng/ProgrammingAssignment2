## The functions defined here use lexical scoping and the properties of a function's closure 
## to compute and cache the inverse of a square matrix. 

## The function makeCacheMatrix creates a special "matrix" object to cache the inverse of 
## an input matrix. This object is a list of "get" and "set" functions, enabling the function 
## cacheSolve to assign or access the values of both the data and the matrix inverse.  
## In makeCacheMatrix, the matrix inverse is initialized to a zero matrix. 
## The inverse of a square matrix of any dimension can be computed. 

makeCacheMatrix <- function(xmatrix = matrix(numeric(0), 0, 0)) {
        inverse <- matrix(numeric(0), 0, 0)
        set <- function(y) {
                xmatrix <<- y
                inverse <<- matrix(numeric(0), 0, 0)
        }
        get <- function() xmatrix
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix in the list object returned by 
## makeCacheMatrix above. If the inverse has already been calculated, then the function 
## cachesolve retrieves the inverse from the cache. If the inverse has not been calculated, 
## or a matrix with different data is input, then a new inverse is calculated. In the function 
## cacheSolve, this is tested using the sum of all matrix elements. (The inverse was initialised
## to the zero matrix in makeCacheMatrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!sum(inverse) == 0) {
                message("getting cached data")
                return(inverse)
        } else {
                matrixdata <- x$get()
                inverse <- solve(matrixdata, ...)
                x$setinverse(inverse)
                inverse}
}
