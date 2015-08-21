## These two functions together restores the reverse of the matrix that have been 
## computed and rretrive that data if needed again without do the same calculation again.
## Calculate the reverse of the maxtrix if previous data is not available.

## This function is a list of functions, including get(), set(),setreverse() and getreverse()

makeCacheMatrix <- function(x = matrix()) {
        r <- matrix()
        set <- function(y) {
                x <<- y
                r <<- matrix()}
        get <- function() x
        setreverse <- function(reverse) r <<- reverse
        getreverse <- function() r
        list(set = set, get = get,setreverse = setreverse,getreverse = getreverse)

}


## This function is used to retireve the data that has been stored in makeCacheMatrix()
## If such data is not available, it will calculate the reverse of the given matrix directly.

cacheSolve <- function(x, ...) {
        r <- x$getreverse()
        if(!identical(r,matrix())) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data,...)
        x$setreverse(r)
        r
}
