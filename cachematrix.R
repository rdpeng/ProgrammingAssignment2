## The first function creates a special "matrix" object that can cache its inverse.
## The second computes the inverse of the matrix returned by the first function. 
## If the inverse has already been calculated,then the second should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the data and 
## sets the inverse in the cache via the setSolve function.

## Write a short comment describing this function
##1.set the matrix
##2.get the matrix
##3.set the inverse of the matrix
##4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix<-NULL
        set <- function(y = matrix()) {
                x <<- y                
        inverse_matrix <<- NULL            
        }
        get <- function() x
        setsolve <- function(solve)  inverse_matrix<<- solve
        getsolve <- function() inverse_matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
##1.determine whether the inverse of a matrix in the cache. 
##2.If not null, extract it from the cache.
##3.Otherwise, it computes the inverse the matrix via setSolve function

cacheSolve <- function(x, ...) {
        inverse_matrix<- x$getsolve()
        if(!is.null(inverse_matrix)) {#条件是m不是“空”的
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix<- solve(data, ...)
        x$setsolve(inverse_matrix)
        inverse_matrix    ## Return a matrix that is the inverse of 'x'
}
