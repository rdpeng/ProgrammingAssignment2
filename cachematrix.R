## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

## To do:
## Change comments from vector to matrix in makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {

    #set variable inv (solve in this case) to NULL
    inv <- NULL
    
    #set function sets x to the argument y and set inv to null
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    
    #get returns the value of x (argument of makeCacheMatrix)
    get <- function() x 
    
    #sets inv in makeCacheMatrix to solve (argument of makeCacheMatrix)
    setinv <- function(solve) inv <<- solve
    # getinv returns the value of inv (from makeCacheMatrix)
    getinv <- function() inv
    
    #returns a labeled vector of functions set, get, setmean and getmean
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #attempts to get the inv from x (if it was calculated previously)
   inv <- x$getinv()
    
    #if not null, a valued was cached, so return inv 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #since its null, set data to x from makeCacheMatrix
    
    data <- x$get()
    
    #calculate the inverse of data
    inv <- solve(data, ...)
    
    #set inv in x to calculated inverse
    x$setmean(inv)
    
    #return inverse
    inv
}
