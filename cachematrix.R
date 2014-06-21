## ----------------------------------------------------------------------
## Set up sample Matrix suitable for inverting and testing functions
##      Matrix Scale
        nValue = 4
##      Create a Matrix
        m <- matrix(rnorm(nValue*nValue), nValue, nValue)
## ----------------------------------------------------------------------
## Code Check - V
## -----------------------------------------------------------------------
## makeCacheMatrix - A function to cache an "Inverted Matrix"
## -----------------------------------------------------------------------

makeCacheMatrix <- function(x) {
        ## initialize value
        Inverted.Matrix <- NULL
        ## define set function 
        set <- function(y) {
                x <<- y
                Inverted.Matrix <<- NULL
        }
        ## define get function - returns source matrix
        get <- function() x
        ## define set inverted Matrix function - caches 
        ## inverted matrix result from solve function
        setImatrix <- function(x) Inverted.Matrix <<- solve(x)
        ## define get Inverted matrix fuction - retures cached inverted matrix
        getImatrix <- function() Inverted.Matrix
        
        list(set = set, 
             get = get,
             setImatrix = setImatrix,
             getImatrix = getImatrix
             )
}



## ----------------------------------------------------------------------
## cacheSolve - A function to fetch an Inverted Matrix. It performs a 
##              check to determine if the cache exist, if not the 
##              function will invert the matrix and store it in the 
##              the cache for later recall
## ----------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        m <- x$getImatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        
        data <- x$get()
        m <- solve(data, ...)
        x$setImatrix(m)
        m
  
}


## ----------------------------------------------------------------------
## Unit Test Code for: makeCacheMatrix
## ----------------------------------------------------------------------
        mcm <- makeCacheMatrix() #initialize function
        mcm$set(m) ## Set Original Matrix
        mcm$setImatrix(m) ## Set Source Matrix as an Inverted Matrix into Cache State
## Get OriginalSource Matrix
        OriginalMatrix <- mcm$get()
        OriginalMatrix

## Get Inverted Matrix from Cache State
        CachedInvertedMatrix <- mcm$getImatrix()
        CachedInvertedMatrix


## Unit Test Scripts
## Get cached matrix from cacheSolve fuction 
        cacheSolve(mcm)



## Get original matrix invformation
        mcm$get()
        mcm$getImatrix()

