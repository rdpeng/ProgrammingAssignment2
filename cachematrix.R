#martin assignment 2
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#instructions: This function creates a special "matrix" object that can cache its inverse.

#explanation
# x initialized as a matrix in the function argument
#inv is initialized with "null"
#set function (make x (already initialized be set to "y", null inv))
#set and get functions
#create least of functions. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Write a short comment describing this function
#instructions: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

#explanation:
#getsinv, if this is not null, it returns the the inv (inverse matrix, which has been cached)
#if there is no inverse it gets the data, then solves matrix (inverts it)
#it sets this inv
#returns the inv
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
# end assignment 2 (the above functions tested succesffully with 2x2 matrix)#