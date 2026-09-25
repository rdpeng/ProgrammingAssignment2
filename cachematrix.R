## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special matrix object that can cache its inverse
## Returns four functions: set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) {
        # Stores the cached inverse
        inv<- NULL
        # Assigns and clears cached inverse
        set<- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Returns current matrix
        get<-function() x
        # Stores inversed matrix in cache
        setinv<-function(inverse) inv<<- inverse
        # Retrieves cached inverse
        getinv<-function() inv
        # Return list of the four functions created
        list(set = set, get = get,
             setinv=setinv,
             getinv=getinv)
}


## Write a short comment describing this function
## Compute the inverse of the matrix created by makeCacheMatrix
## If already cached, return the cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # Gets cached inverse
        inv<-x$getinv()
        # IF cached inverse exists, return it
        if (!is.null(inv)) {
                return(inv)
        }
        # Else compute the inverse
        mat<-x$get()
        inv<-solve(mat, ...)
        x$setinv(inv)

        inv
}
