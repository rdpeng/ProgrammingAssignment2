
## The first function,  creates our matrix as we have defined while calling from the console, having list of 4 function which are 1.set the matrix 2. get the matrix 3. set the inverse 4. get the inverse


makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The second function calculates the inverse of the matrix defined above and sets the value of the inverse in the cache through the setInverse function.

cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
