## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  function creates a "special matrix" & caches it
##  ... for retrieval later via LEXICAL SCOPING !
##   has 4 embedded functions in its list:
## in list: format of each is  <name> = <name>
## get x, set x, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {  ## x is <main> 
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {x}
    setinv <- function(inv) { i <<- inv }
    getinv <- function() {i}
    list(set = set, get = get, 
         setinv = setinv
         getinv = getinv)
}

## Write a short comment describing this function
## 'cacheSolve' function computes the inverse 
## of the "special matrix"
## if the inverseMatrix has already been calculated 
## (i.e., and the matrix has not changed), 
## then cacheSolve RETRIEVES the cached inverse
## INSTEAD of doing the long inversion calculation

cacheSolve <- function(x, ...) {   ##  why the "..." needed?
i <- x$getinv()    ##format:  <main>$<f2>   ** no arguments
    if( ! is.null(i)) {
            message("getting cached matrix, KSF")
            return(i)
    }  
    matrixinput <- x$get()
    i <- solve(matrixinput, ...)  ## 'i' becomes inverted matrix
                                    # "solve(y)" inverts matrix    
                                    ##  why the "..." needed?
    x$setinv(i) 
                ## call subf of main "x" funct TO 'cache' for use later
    i           ## Return a matrix that is the inverse of 'x'
}  ## ksf1
