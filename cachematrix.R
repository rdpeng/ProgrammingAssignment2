# My solution
# The function calculates the inverse of a matrix
# but if that already has been done for a matrix then,
# i simply retrieves the solution from the cache... Duh

# first I create the special "vector"

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
        ## Return a matrix that is the inverse of 'x'
}
