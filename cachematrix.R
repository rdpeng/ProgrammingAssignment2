## "makeCacheMatrix" and "cacheSolve" are functions that 
## allow store of inverse of a matrix this allows us to only only calculate it once

## "makeCacheMatrix" creates a matrix and creates 4 functions to set and get
## information from the matrix.

## 'set' assigns a value to the matrix
## 'get' returns the original matrix
## 'setinvrs' assigns a value to inverse matrix
## 'getinvrs' prints the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## creates matrix object capable of cashe
        invrs <- NULL ## for now sets the inverse as null
        
        set <- function(y) {
                x <<- y
                invrs <<- NULL ##indicates new matrix
        }
        
        get <- function() x ## returns original matrix
        
        getinvrs <- function() invrs ## returns the stored inverse matrix
        
        setinvrs <- function(inverse) invrs <<- inverse
        
        ## returns a list of functions
        list(set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
        
}


## cacheSolve calculates a value for the invserse matrix of a matrix created by 'makeCacheMatrix'
## it is then stored in the inverse matrix field. the function then searches. If the
## inverse was previously calculated and matrix was not changed, the inverse matrix is printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinvrs() # gets inverse value of a matrix
        
        if (!is.null(invrs)){
                message("getting cached data")
                return(invrs) ## if the inverse value is found in cache it returns it
        }
        
        data <- x$get() ## gets matrix to calculate inverse
        inverse <- solve(data)  ## calculates inverse
        x$setinvrs(inverse)  # inverse matrix stored
        
        invrs ## returns a matrix tht is the inverse of x
}

