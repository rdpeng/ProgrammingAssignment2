## The overall objective of the two functions below is to reduce computation 
## times in situations where there may be cause to calculate the inverse of a 
## matrix more than once. The functions are written to take advantage of lexical
## scoping in R, by caching a target object within the envionment of the 
## matrix to be anaylsed. 
## Using makeCacheMatrix(), the object `inv` is cached within the matrix 
## environment.On it's first run, cacheSolve() calculates the inverse of 
## this matrix and assigns it to `inv`. Subsequent to the first run of 
## cacheSolve, this function will then call the pre-existing value for inv 
## (cached in the matrix envionment) rather than calculate it again. This saves 
## considerable time as calculating the inverse of a matrix is heavy, 
## computationally.



## Function 1
## makeCacheMatrix() is used to create a matrix with a value named `inv` cached 
## within the matrix environment, where inv is set to null initially. It is 
## written to allow the second cacheSolve() function to interact with it, 
## via a list of predefined terms

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Assign a matrix with a cached object, named `inv`


## Function 2
## -cacheSolve() examines the resulting matrix from `makeCacheMatrix` to see 
## whether the cached value `inv` is something other than NULL. If TRUE (which 
## would indicate that the matrix has already been subjected to the cacheSolve()
## function), the function will retrieve the value already attributed to inv and
## print it. 
## If inv is NULL (i.e. !is.null(inv) = FALSE) the script will pass over the `if` 
## command and calculate the inverse of the matrix, assign it to `inv` which is 
## cached in the matrix environment, and then print the value of `inv`. Assuming 
## the contents of this matrix do not change, the pre-cached value for `inv` will 
## remain correct each time cacheSolve() is run.

cacheSolve <- function(x, ...) {
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
## Return a matrix that is the inverse of 'x' 
## (when x is not not a singular matrix, i.e. it's invertible mathematically)