## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Matrix inverse is time-comsuming computations. I write a function
## to cathe the inverse result. If the matrix doesn't change,
## the inverse can be looked up in the cathe rather than recomupted.

## below I write makeCacheMatrix to cathe the inverse result

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
	     get = get,
	     setInverse = setInverse
	     getInverse = getInverse) 

}


## Write a short comment describing this function
## The function below computers the inverse of a matrix. If the inverse has already been calculated and the matrix has not changed, then it should get the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        	message("getting cached results")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
