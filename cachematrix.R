## Put comments here that give an overall description of what your
## functions do

## in this fun create special matrix to sore the inverse in it

makeCacheMatrix <- function(x = matrix()) {
        ##set inverse to null when insialize the matrix
        inv <- NULL
        ##set the matrix data
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ##get the matrix data
        get <- function() x
        ##set the inverse of the matrix
        set_inv <- function(solve) inv <<- solve
        ##get the inverse of the matrix
        get_inv <- function() inv
        ##return the special matrix
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## check if the matrix inverse 
##exist or not if exit ith show with a message getting cached matrix
## if not caculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse
        inv <- x$get_inv()
        ##check if inverse cached or not
        if(!is.null(inv)) {
                ## if cached return it
                message("getting cached matrix")
                return(inv)
        }
        ##if not cached calculate it, cach it then return it
        inv<- solve(x$get())
        x$set_inv(inv)
        inv
}