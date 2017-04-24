## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##In this first step we initialize objects x (as a function argument) and inv as Null
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
     ## With the set function we assign y to the object x and Null to inv, but in the parent enviroment.
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     ##x is not in  the parent enviroment, so we use get
     get <- function() x
     ##Analogous steps applied to the inv matrix (set and get them and assign them to objects in the parent enviroment)
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     ##Now we assign this functions to elements of a list an rename them as set, get, setInverse and getInverse.
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##We assignt the x$getInverse() of the previous function to inv
        inv <- x$getInverse()
        
     ##The next part of the code is written to check if inv matrix is null.
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     ##If the condition above is False, we now get x, calculate its inverse and use de setInverse function describe before.
     ##Print inv
     data <- x$get()
     inv <- inverse(data, ...)
     x$setInverse(inv)
     inv
}
