## First line is creating the matrix and then next setting the data to equal x
## Line 13 getting the data to retrieve in the data <- x$getdata for cacheSolve 
## function
## Line 14 - data will be entered when cacheMatrix is solve
## Line 16-18 making a list of functions to name and call open for CacheSolve

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL   
  set <- function(y) {  
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv       
  list(set = set, get = get,            
       setinverse = setinverse,
       getinverse = getinverse)
}


## First the function is looking to see if the inverse is already cached. If it
## is, it will print getting cached data and return the inverse in cache
## if it isn't cached, it will get the data created when the makeCacheMatrix was 
## run to solve the the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
