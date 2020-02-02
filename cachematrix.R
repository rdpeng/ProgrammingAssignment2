# My solution
# The function calculates the inverse of a matrix
# but if that already has been done for a matrix then,
# i simply retrieves the solution from the cache... Duh

# first I create the special "vector" to store the matrix (i.e. create a list containing a function to:
# set value of the matrix, get value of the matrix, set value of the inverse, get value of the inverse

makeCacheMatrix <- function(x = matrix()){
  m <-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Then I create the function that either calculates the inverse 
# or if answer already stored
# it retrieves it from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
} 

# Tadaaaa
# Thank you, good luck and have fun :)
