## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 -		 +  inversa <- NULL
 +  set <- function(y) {
 +    x <<- y
 +    inversa <<- NULL
 +  }
 +  get <- function() x
 +  setInverse <- function(inverse) inversa <<- inverse
 +  getInverse <- function() inversa
 +  list(set = set, get = get,
 +       setInverse = setInverse,
 +       getInverse = getInverse)
  }		  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 -        ## Return a matrix that is the inverse of 'x'		 +  inversa <- x$getInverse()
 +  if(!is.null(inversa)) {
 +    message("getting cached data")
 +    return(inversa)
 +  }
 +  data <- x$get()
 +  inversa <- solve(data, ...)
 +  x$setInverse(inversa)
 +  inversa
  }		  }
