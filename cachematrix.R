## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        
        setmatrix <- function(y){
                x <<- y
                inv <<- matrix(ncol = ncol(y), nrow = nrow(y))
        }
        setinv <- function(inverse) inv <<- inverse
        
        getmatrix <- function() x
        getinv <- function() inv

        list(setmatrix = setmatrix, setinv = setinv, 
             getmatrix = getmatrix, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        mat <- x$getmatrix()
        inv <- x$getinv()

        if(sum(is.na(inv))==0){
                return(inv)
        }
        
        b <- diag(nrow(mat))
        inverse <- solve(mat, b)
        x$setinv(inverse)

        return(inverse)
}


