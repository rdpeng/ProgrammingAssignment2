## This part of functions saves time on a computation that typically takes a lot of computing power. Rather than needing to calculate the inverse of a matrix every single time, these functions first create an object capable of storing a matrix and then cache its inverse. 

## This function creates an object that can cache its inverse. 

makeCacheMatrix <- function(mtrx = matrix()) {
inv <- NULL
set<- function(y){
	mtrx<<-y
	inv<<- NULL
}
get<- function() mtrx
setInverse<- function(inverse) inv <<- inverse
getInverse<- funtction() inv
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##This function computes the inverse of the object returned by
## makeCacheMatrix. If the inverse has already been calculated and 
## stored, then the cachesolve should retrieve the inverse from the cache
## instead of recalculating.
cacheSolve <- function(mtrx, ...) {
        inv <- mtrx$getInverse()
        if (!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        mat<- mtrx$get()
        inv<- solve(mat, ...)
        mtrx$setInverse(inv)
        inv
}
