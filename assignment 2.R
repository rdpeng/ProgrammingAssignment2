## My motto in this experiment is to write a pair of functions name makeCacheMatrix and cachesolve that caches the inverse of matrix
## makeCacheMatrix is a function which creates a special 'matrix" object that 
 can cache its inverse for the input which is an ivertible square matrix. 
makeCacheMatrix <- function(x = matrix()) {

}
 inv <- NULL
 set <-function(y) {
 x <<-y
 inv <<-NULL
}
get <- function()x
setinv <- function(inverse)inv <<-inverse
getinv <- function()inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## cacheSolve is a function which computes nothung but the inverse of the 
special "matrix" returned by the makecacheMatrix above.
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
inv <- x$getinv()
if(!is.null(inv)) {
message("getting cached result")
return(inv)
}
data <- x$get()
inv <- solve(data,...)
x$setinv(inv)
inv
}
