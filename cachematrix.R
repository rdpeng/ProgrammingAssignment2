## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## With the help of two supporting functions, named makeCacheMatrix() and 
## cacheSolve(), here it gives an implementation of functions that cache 
## the inverse of a matrix. If inverse if already present, it is returned as 
## previously computed.

##
## This function creates a special "matrix" object that can cache its inverse.
## Supporting setter/getter methods are given to set the solved inverse into
## caller object.
##
## Arguments
## x: the input Matrix object
##
## Returns
## Special matrix object
##
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function computes the inverse of the special matrix obj made by 
## makeCacheMatrix function above. If the inverse of the given matrix has already 
## been calculated in past, then the cacheSolve function should retrieve the 
## inverse from the cache and return, instead recalculating.
##
## Arguments
## x: the input Special Matrix object
##
## Returns
## Cached inverse for given argument matrix
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Sample Output:
# ===============
#
# > mat<-makeCacheMatrix()
# > mat$get()
# [,1]
# [1,]   NA
# > mat$set(matrix(rnorm(4,2,2),2,2))
# > mat$get()
#             [,1]    [,2]
# [1,] -0.88416134 7.21396
# [2,] -0.01730152 2.29078
#
# > mat$getinverse()
# NULL
#
## Above result comes from the cache of mat special matrix object
#
# > cacheSolve(mat)
#              [,1]      [,2]
# [1,] -1.205288834 3.7956086
# [2,] -0.009103154 0.4651995
#
# > mat$getinverse()
#              [,1]      [,2]
# [1,] -1.205288834 3.7956086
# [2,] -0.009103154 0.4651995
#
## Above result comes from the cache of mat special matrix object