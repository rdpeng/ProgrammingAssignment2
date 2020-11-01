## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  
  set <- function(y) {
    
    x <<- y
    k <<- NULL
    
  }
  
  get <- function() x
  setinversa <- function(inversa) k <<- inversa
  getinversa <- function() k
  list(
    setinversa = setinversa,
    getinversa = getinversa,
    set = set,
    get = get
  )
  
}


## Write a short comment describing this function

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinversa()
  if (!is.null(k)) {
    
    
    message("getting cached data")  
    return(k)
    
  }
  Pr <- x$get()
  k <- solve(Pr,)
  x$setinversa(k)
  k
}