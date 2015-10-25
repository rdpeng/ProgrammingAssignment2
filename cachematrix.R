# Similar to the mean of a vector example, the makeCacheMatrix follows similar goals:
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

# Similar to the second part of the vector example, again replacing mean with inverse/solve
# The following function calculates the inverse of the special "matrix" created above, however it
# 1. Checks if the inverse is already calculated
# 2. If so, get the inverse from the cache and skip computation
# 3. If not, calculate the inverse of data and sets the value of the inverse in the cache via setinverse function

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

# The blog post linked to in the forum was quite informative too.