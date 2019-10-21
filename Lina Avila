## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(), ... ) {  
inver <- NULL 
set <- function(y) {
x <<- y 
inver <<- NULL 
} 
get <- function () x 
setInverse <- function (inverse) inver <<- inverse #calculate the inverse 
getInverse <- function() inver 
list(set= set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Return a matrix that is the inverse of `x´

cacheSolve <- function(x, ...) {
inver <- x$getinverse()
if(!is.null(inver)) {
message ( "getting cached data") 
return (inver) 
} 
data <- x$get() 
inver <- solve(data, ...) 
x$setinverse(inver) 
inver 
}
