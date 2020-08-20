## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) { 
invt <- NULL 
set <- function(y) 
{
x <<- y 
invt <<- NULL 
} 
get <- function() x 
setInverse <- function(inverse) inv <<- inverse 
getInverse <- function() invt 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}
cacheSolve <- function(x, …) { 
invt <- x$getInverse()
if(!is.null(invt)){
message("gettingcacheddata")
return(invt)
}
mdata<−x$get() 
invt <- solve(mdata,...) 
x$setInverse(invt) 
invt
}
