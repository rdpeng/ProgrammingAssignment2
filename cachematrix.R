## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function(){ x }
	set_inverse <- function(x_inv){ i <<- x_inv}
	get_inverse <- function(){ i }
	
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	i <- x$get_inverse()
	
	if(is.null(i)) {
		my_matrix <- x$get()
		i <- solve(my_matrix)
		x$set_inverse(i)
	} else {
		message("getting cached data")
	}
	
	i
}
