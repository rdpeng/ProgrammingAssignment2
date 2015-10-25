## The function creates a list of functions which is used to store the input matrix 
## and its inverse. Set and setinv is used to assign the matrices and get and getinv 
## is used to retrive the matrices from the cache
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function will give us the inverse of the input matrix either by computing it using 
## 'solve'or by retrieving it from cache 
## if the inverse it retrieves using getinv() is null then only it computes the inverse else 
## we get the cached data which is then stored using setinv()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
