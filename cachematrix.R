## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

## To do:
## Change comments from vector to matrix in makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
{
    #set variable inv (solve in this case) to NULL
    inv <- NULL
    
    #set function sets x to the argument y and set m to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get returns the value of x (argument of makeVector)
    get <- function() {
        x
    } 
    
    #sets m in makeVector to mean (argument of makeVector)
    setinv <- function(solve){
        inv <<- solve
    }
    
    # getmean returns the value of m (from makeVector)
    getinv <- function() {
        inv
    }
    
    #returns a labeled vector of functions set, get, setmean and getmean
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
