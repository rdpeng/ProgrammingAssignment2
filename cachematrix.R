#x is initialized as function argument, x is an matrix
#i is set to null
#set takes an argument that is named as y
#assignment operator assigns the value of right side of the operator to the object in parent environment on left side
# assign input argument to x in parent environment and assign value null to i in parent environment 
# setinverse completes, assignment operator used to assign the input argument to value of m in parent environment
#getinverse gets the value


makeCacheMatrix <- function(x = matrix()) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse)i <<- inverse
getinverse <- function() i
list(set = set, 	
get = get,		
setinverse = setinverse,	
getinverse = getinverse)
}

#solve function used to calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
#calls the get inverse function on input object
#checks if the result  returns the inverse value,if the result is not already calculated
if(!is.null(i)) {
message("getting cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
        ## Return a matrix that is the inverse of 'x'
}
