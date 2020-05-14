## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL # set inverse = NULL
set <- function (y) { # set (variable) = y
        x <<- y # assigning y into x
        inv <<- NULL # assigning null into inv
   }
 get  <- function() x # assigning function of x into get
        setInverse <- function(solveMatrix) inv <<- solveMatrix # matrix
        getInverse <- function() inv   # assigning fun into get inverse
        list(set = set, get = get, # list
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
        if(!is.null(inv)) { # conditional statement
                message("getting cached data")  # print message
                return(inv) # return if true
        }
        data <- x$get()  # get
        inv <- solve(data, ...) # inverse
        x$setInverse(inv) #set
        inv
}
