## Put comments here that give an overall description of what your
## functions do
    ## Matrix inversion is usually a costly computation and there may be some 
    ## benefit to caching the inverse of a matrix rather than computing it repeatedly

## Write a short comment describing this function
    ## The function makeCacheMatrix(x) creates a special "matrix", 
    ## which is really a list containing a function to :
    ## set() to set the value of matrix 
    ## get() to get the value of matrix 
    ## setsolve() to set the inverse matrix
    ## getsolve() to get the inverse matrix
    ## note also that makeCacheMatrix(x) expects a matrix as the x parameter.
    ## im --> inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) im <<- solve
    getsolve <- function() im
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
    ## Cachesolve function provides matrix's inverse calculation or retrieval 
    ## from memory if the inverse matrix has been already calculated
cacheSolve <- function(x, ...) {
    im <- x$getsolve()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setsolve(im)
        ## Return a matrix that is the inverse of 'x'
    im
}

## Example: I tried this. works fine  
x<- matrix(c(3,4,2,5),2,2)
example_matrix <- makeCacheMatrix(x)
cacheSolve(example_matrix)
