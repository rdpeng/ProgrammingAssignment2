##Luis Fernando Monterrubio Cota

## My cache matrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  get <- function()matrix
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function()i
  list(set = set, get = get,
       setm = setm,
       getm = getm)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)%*% data
  x$setinverse(m)
  m
}
