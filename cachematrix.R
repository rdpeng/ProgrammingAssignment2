makeCacheMatrix <- function(M=matrix()){
	## creates a special "matrix" object that can cache its inverse
	inv <- NULL	
	set <- function(y){
		M <<-y
		inv <<- NULL
	}
	get <- function() M
	setinverse <- function(z) inv <<- z
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(M, ...){
	## computes the inverse of the special "matrix" returned by makeCacheMatrix. if the inverse has already been cmoputed, then it retrieves the inverse from the cache
	inv <- M$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- M$get()
	inv <- solve(data, ...)
	M$setinverse(inv)
	inv
}

## Test the functions
#mat <- matrix(1:4,2,2)
#aMatrix <- makeCacheMatrix(mat)
#aMatrix$get()
#aMatrix$getinverse() # NULL
#cacheSolve(aMatrix)
#solve(mat)
#aMatrix$getinverse()
#aMatrix$set(30:50)
#aMatrix$getinverse()
#cacheSolve(aMatrix)
#aMatrix$getinverse()


