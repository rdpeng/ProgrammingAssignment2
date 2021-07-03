
# Simply set x as the input matrix.
#Solve "inv" as a NULL.
# Instead of using other variables asides from the text, just use it as it is. 
# One can also change if wanted to. 


makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- fucntion(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Do remember the variables set in every line.

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...) 
    x$setInverse(inv)
    inv
}
