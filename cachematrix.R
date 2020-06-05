## Below are the functions that make the process of solving a matrix in a simple and fast way by avoiding the 
# recomputation of the values coming in the loop again and again
# FOr this, we have two functions which ultimately store all the values that are repeated in a loop in cache memory
# which will help in easy retrieval of the required data by looking directly in the cache memory

## Below function creates a vector that comprises of a set of functions that deliver a matrix. 
# Such as set is a function thats sets the value of the matrix, get is the function which will recall the matrix data
# Similarly, setinverse and getinverse are the fucntions which will set the inverse values of the data fed to the 
# matrix and getinverse will recall and print those values.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cachesolve is a function that basically stores the already computed values and prints them when called.
# By calling the above function, the console is printed with the required computed values and prints a message as 
# mentioned below in the function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
