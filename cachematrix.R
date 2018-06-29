## The function makeCacheMatrix initializes the variables and returns the list of functions
## which are then used by function cacheSolve to get and set the inverse of matrix. 

## This function makeCacheMatrix returns the list of below mentioned functions. It also initializes
## matrix and caches the value of the inverse of the matrix

## getmatrix - returns the original matrix
## setinv - sets the inverse of the matrix
## getinv - returns inverse of the matrix
## set - assigns matrix passed as the argument and stores it in the parent environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }

        getmatrix <- function(){ 
                x
        }
        setinv <- function(i) inv <<- i
        getinv <- function()inv
        list(set = set, getmatrix = getmatrix , setinv = setinv, getinv = getinv)
        
}


## Returns inverse of matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        print("INV = ") 
        inv
        if(!is.null(inv)){ ## checks if the inverse is not NULL, then returns its cached value
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$getmatrix()
        inv <- solve(mat) ## calculates inverse of the matrix
        x$setinv(inv) ## sets inverse of the matrix in cache for future use
        inv ## Return a matrix that is the inverse of 'x'
}
