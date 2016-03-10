## Put comments here that give an overall description of what your
## functions do

## in this fun create special matrix to sore the inverse in it

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inv <<- solve
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## check if the matrix inverse 
##exist or not if exit ith show with a message getting cached matrix
## if not caculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        inv<- solve(x$get())
        x$set_inv(inv)
        inv
}