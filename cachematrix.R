## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                         ## inv as NULL     
set <- function(y) {                ## defining the set function       
x <<- y                             ## value of matrix
inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
}
get <- function() x                     ## define the get function - returns value of the matrix argument
setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv
getinverse <- function() inv                     ## gets the value of inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv
}

