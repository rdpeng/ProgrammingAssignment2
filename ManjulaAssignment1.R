CacheMatrix <- function(x=matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}


man1 <- CacheMatrix(matrix(1:4,2))
man1$get()
man1$getInverse()
man1$set(matrix(5:8,2))
man1$get()
cacheSolve(man1)
cacheSolve(man1)
man1$getInverse()
man2 = man1$getInverse()
man1$get() %*% man2
