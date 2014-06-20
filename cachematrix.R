## makeCacheMatrix is a function which creates a special "matrix" 'x' 
## and caches its inverse 'inv' (initially set to NULL) 
## by returning a list with 4 function elements:
##
## set()	-	creates a matrix x by assigning values of 'y'
##	"<<-"	-	causes a search to be made through parent 
##			environments for an existing definition of 'x'.
##			If found, 'x' is redefined, 
##			else assignment happens in the global environment.
##
## get()	-	returns the newly created matrix 'x'
##
## setInv()	-	redefines 'inv' to the cached inverse if it has already been 
##			defined and matrix 'x' has not changed 
##			else 'inv' is kept as NULL
##
## getInv()	-	returns the cached inverse of the matrix 'x' if exists 
##			else NULL

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(cacheInv) inv <<- cacheInv
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve is a function which computes the inverse of the "matrix" 'x' 
## returned by 'makeCacheMatrix' function
## 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve retrieves the inverse from the cache and return it
## Else it computes and returns the inverse of the matrix using the solve() function 
## and calls the 'setInv' function of 'makeCacheMatrix' function to cache it.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInv(inv)
        inv
}
