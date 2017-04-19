## Those functions cache the inverse of matrix and store it in an object

##makeCacheMatrix create an object to store matrix and there inverse in cache
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the inverse is not stored in cache cacheSolve calculates and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	y <- x$getinverse()
	if (!is.null(y)){
		print("get the cache")
		return(y)
	}
	m <-x$get()
	inv <- solve(m)
	x$setinverse(inv)
	inv
}
