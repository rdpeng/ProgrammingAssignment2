## ============================
## Caching the Inverse Function
## ============================
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## These functions cache the inverse of a matrix
##


## ============================
## makeCacheMatrix (x)
## This function creates a special "matrix" object that can cache its inverse
## ============================
##
## ARGUMENTS: 
## x: a matrix 
## (NOTE: this function will assume the matrix is inversible. i.e. it will NOT check for this)
##
## FUNCTIONS:
## set()
## get()
## setInverse()
## getInverse()
##

makeCacheMatrix <- function(x = matrix()) {
        ## variables in makeCacheMatrix to keep track of attributes
        ## ========================================================

        inverse <- NULL
        changed <- FALSE
        
        ## the functions that set and get the matrix
        
        set <- function(y) {            ## the set function takes argument (y)
                x <<- y                 ## sets the matrix to y
                inverse <<- NULL        ## resets the inverse (to NULL)
                changed <<- TRUE        ## resets the "changed" attributes (to TRUE)
                                        ## NB "changed" is used to indicated that the matrix has 
                                        ##   changed (and the inverse function has not been called after that (yet))
        }
        
        get <- function() x             ## the get function, returns the matrix
        
        setInverse <- function(solve) { ## sets the inverse of the matrix
                changed <<- FALSE       ## sets the "changed" attribute to FALSE (for use in cacheSolve)
                inverse <<- solve       ## calculates the inverse matrix and sets the result to "inverse"
        }
        
        getInverse <- function() inverse        ## returns the "inverse" matrix
        
        matrixChanged <- function() changed     ## returns whether the matrix "changed"
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse, matrixChanged = matrixChanged)
}


##===============================
## cacheSolve: This function computes the inverse of the special "matrix"
##             returned by makeCacheMatrix above. If the inverse has already
##             been calculated (and the matrix has not changed), then the
##             cacheSolve should retrive the inverse from the cache.
## ==============================
##
## ARGUMENTS:
## x: special "matrix" object that can be created by makeCacheMatrix function above
##

cacheSolve <- function(x = matrix(), ...) {
## This Function Returns a matrix that is the inverse of 'x'

        ## Check to see if prior conditions met 
        
        inverse <- x$getInverse()                       ## retrieves the inverse of the matrix supplied
        if(!is.null(inverse) & !x$matrixChanged()) {    ## if already calculated AND not changed
                message("getting cached data")          ## load cached inverse
                return(inverse)                         ## return cached inverse
        }
        
        ## the following code runs if above conditions FALSE
        ## i.e. the inverse matrix NOT calculated yet AND the matrix has NOT changed
        
        data <- x$get()                                 ## load the matrix
        inverse <- solve(data, ...)                     ## calculate the inverse
        x$setInverse(inverse)                           ## set the calculated inverse
        inverse                                         ## return the (calculated) inverse
        }

