## We are writing a function to create the inverse of a matrix(assuming it can be calculated). If the inverse has already been calculated, it gets
## retrieved from the cache and returned as result. This saves calculation effort which, for matrix inversion, can be computationally expensive.

## makeCacheMatrix function creates a list containing a function to 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the vector created with the above function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
