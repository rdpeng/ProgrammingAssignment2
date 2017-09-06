## Put comments here that give an overall description of what your
## functions do


# The makeCachematrix function creates a matrix and caches it.
# The cacheSolve function uses makeCacheMatrix to solve the inverse
# of the matrix. If it is not cached, then cacheSolve calculates the inverse.

## Write a short comment describing this function

# makeCacheMatrix caches the matrix, it has a function to set and get 
# the value of the matrix.


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set,
             get = get,
             setinverse = setinverse
             getinverse = getinverse)
   
        }


## Write a short comment describing this function
# cacheSolve uses the makeCacheMatrix to calculate the inverse. If the
# result is cached then it returns cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("GETTING CACHED DATA")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        
}
