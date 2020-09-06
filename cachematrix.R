makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){ ## set value of the matrix using another function y
    x <<- y      
    i <<- NULL     ## use closer functions (<<-)to access the parameters of parent function .Here, x
  }
  get <- function() {x} ## get the value of the matrix
  setInverse <- function(inverse) {i <<- inverse} ## set value of inverse
  getInverse <- function() {i}  ## get value of the inverse
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)  
}
cacheSolve <- function(x, ...) { ## to compute the inverse of the matrix created.
  i <- x$getInverse() ## returns the matrix that is inverse of 'x' and assign it to 'i'
  if(!is.null(i)){     ## check if the inverse is already calculated to skip computation.
    message("cached data...")
    return(i) ## Return a matrix that is the inverse of 'x'
  }
  m <- x$get()
  i <- solve(m, ...)   ## solve is the standard R function to compute inverse of the matrix
  x$setInverse(i)   ## set value of the inverse of the matrix to the cache.
  i ## print i value.
}
