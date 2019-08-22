## The following fnctions are used to find the inverse
## of a matrix and then cache the inverse for future use until 
## a new value is desired. 


## makeCacheMatrix takes a matrix as an argument and then provides as an output
## a list to be used in calculating the inverse when assigned to an objet. 
## The set function, while not used in the intial call
## can be used to change the value of the matrix solved for in cacheSolve without 
## reinitializing the object.
## 
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mat <<- solve 
    getinv <- function() mat
    list(set = set, get = get, 
                    setinv = setinv, 
                    getinv = getinv)
}

## When a value of makeCashMatrix is assigned to an object, cacheSolve is able to take
## that object as an argument and determine if the inverse has already been calculated. 
## If it has, first inv_mat will be populated from the cached value and displayed with the 
## message.
## If not, it will calculate the new inverse and store that for subsequent calls with the 
## same value in $set(). 
LeesCacheMatrix <- makeCacheMatrix(m1)

cacheSolve <- function(x, ...) {
    inv_mat <- x$getinv()
    if(!is.null(inv_mat)) {
        message("getting cached data")
        return(inv_mat)
    }
    data <- x$get()
    new_inv <- solve(data, nrow = sqrt(length(x)))
    x$setinv(new_inv)
    new_inv
}








