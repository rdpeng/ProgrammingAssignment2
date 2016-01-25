
## This function caches the result of a matrix inverse calculation so that subsequent calls to calculate the inverse don't actually require the full calculation

  ## This function produces a matrix object that has functions tagged to it. These functions include:
    # a constructor (set), 
    # a return-me function (set), 
    # a set inverse function that uses the solve function to calculate the inverse, and
    # a get inverse that returns m, which in this case, is the inverse calculation result.
    # The function returns a list of functions which can be called later on matrices created via makeCacheMatrix

  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
  
  ## CacheSolve checks whether the specific object that we created with the above function has had its inverse calculated yet.
  ## If that calculation has happened already, it simply looks up the inverse using the getinverse function defined above.
  ## If it has not been calculated yet, then it goes ahead and uses the set inverse function defined above 
  cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
  }
  
  ## Testing code
  # B = matrix(c(1, 4, 3, 1, 5, 7, 5, 8, 12), nrow=3, ncol=3)
  # first_M1 <- makeCacheMatrix(B)
  # cacheSolve(first_M1)
  # out1 <- cacheSolve(first_M1)
  