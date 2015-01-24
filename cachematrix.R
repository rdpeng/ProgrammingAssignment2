## makeCacheMatrix() function takes the input as square invertible matrix.
## This function returns a list, which has following functions
#set the value of the vector
#get the value of the vector
#set the value of the i(inverse of matrix)
#get the value of the i(inverse of matrix)



makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y 
		i <<- NULL
	}
	get <- function() x

	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
		setinverse = setinverse, getinverse = getinverse )

}


## cacheSolve function checks whether the inverse of matrix is already calculated or not & returns it accordingly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inverse <- x$getinverse()
	
	  if (!is.null(inverse)){
		message("returning cached inverse")
		return(inverse)
	  }
	
	  matrix <- x$get()
          inverse <- solve(matrix)
	  x$setinverse(inverse)
	  inverse
}
