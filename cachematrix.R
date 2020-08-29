## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCasheMatrix creates an object that can be used to speed up 
# the process of calculating the inverse of a matrix

makeCacheMatrix <- function(magic = matrix()) {
        ci <- NULL #clears previous cached matrix
        set <- function(k) { #stores new value to environment, clears cache
                magic <<- k #making these objects available in a different environment
                ci <<- NULL
        }
        get <- function() magic 
        setinv <- function(solve) ci <<- solve
        getinv <- function() ci
        list(set = set, get = get, setinv = setinv, getinv = getinv) #list of named elements to enable the $ operator
}
## Write a short comment describing this function
# cacheSolve uses the function makeCacheMatrix to speed up the
# calculation process of inverting a given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                ci <- x$getinv()
                if(!is.null(ci)) {
                        message("BRB, snagging it from the back...")
                        return(ci)
                }
                calcm <- x$get()
                ci <- solve(calcm, ...)
                x$setinv(ci)
                ci
        }
