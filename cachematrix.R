# Similar to the mean of a vector example, the caching of the matrix follows similar goals:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

# Very similar to the example, replacing mean w/ inverse
# And m w/ inv for clarity and sanity

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

# Similar to the vector example, except replacing mean with inverse
# Check if inverse is already calculated
# If so, get the inverse from the cache and skip computation
# If not, calculate inverse of data and sets the value of the inverse in the cache via setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # Checks for already calculated inverse
  if(!is.null(inv)) {
    message("getting cached data") #If it's calculated already, get it from cache
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # If not, calculate the inverse
  x$setinverse(inv) # Via setinverse function
  inv
}
