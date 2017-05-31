## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                x_1 <<- NULL
                set <- function(y) {
                      x <<- y
                      x_1 <<- NULL
                }
                get <- function() x
                getsolve <- function() x_1
                setsolve <- function(y) x_1 <<- y
                list(get=get, set=set, getsolve=getsolve, setsolve=setsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_1 <- x$getsolve()
        if (!is.null(x_1)) {
                print('matrix already inversed')
                return(x_1)
        }
        x_1 <- solve(x$get())
        x$setsolve(x_1)
        x_1
}

a1 <- matrix(1:4,2,2)
cm <- makeCacheMatrix(a1)
m1 <- cm$get()
m_1 <- cacheSolve(cm)

