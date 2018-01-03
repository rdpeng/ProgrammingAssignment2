## Matrix inversion is usually a costly computation, 
## and these 2 functions will cache the inverse of a specific matrix
## if it is invertible.

## although this assignment assume the matrix supplied is always invertible,
## I make two "if" tests on the matrix.


## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        if((dim(x)[1]-dim(x)[2])!=0) {
                stop("The matrix must be a square matrix,please enter again")     
        }
        if(det(x)==0) {
                stop("The determinant is 0, please enter again")
        }
        # test before doing inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
        

