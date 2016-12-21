## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
i <- NULL

	set <- function(matrix){
		m <<- matrix
		i <<- NULL
	}

	get <- function(){
		m
	}
	setInverse <- function(inverse){
		i <<- inverse
	}
	getInverse <- function(inverse){
		i 
	}

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       	m <- x$get()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data)
	x$setInverse(m)
	m
}
