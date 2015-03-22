
# makeCacheMatrix is a function that returns a list of functions,
# It has the  puspose of storing a matrix and a cached value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
	i < - NULL
	set <- function(y) {
		x <<- y
		i << - NULL
	}
	get <- function () x
	set_inverse <- function (inv) i <<- inv
	get_inverse <- function () i
	
	
	#return a list of the elements name of the list 
	list (
		set = set, get = get, set_inverse = set_inverse,get_inverse = get_inverse
	
	)

}


## This function calculate the inverse of a matrix that was created with the MakeCacheMatrix function

cacheSolve <- function(x, ...) {
	i <- x $get_inverse()
	if (!is.null(i)){
		message("getting cached data")
		return (i)
	}
	
	m <- x$get()
	i <- solve(m, ...)
	x$set_inverse(i)
	i    #return the inverse
        
}
